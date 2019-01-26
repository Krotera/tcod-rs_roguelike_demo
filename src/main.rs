extern crate tcod;
extern crate rand;
// serde related crates for save data [de]serialization + JSONification
// https://docs.serde.rs/serde/trait.Serialize.html
// https://docs.serde.rs/serde/trait.Deserialize.html
extern crate serde;
#[macro_use] extern crate serde_derive; // Use serde_derive's macros
extern crate serde_json;

use std::cmp;
use std::io::{Read, Write};
use std::fs::File;
use std::error::Error;

use tcod::console::*;
use tcod::colors::{self, Color}; // roguecentral.org/doryen/data/libtcod/doc/1.5.1/html2/color.html
use tcod::input::{self, Event, Key, Mouse};
use tcod::map::{Map as FovMap, FovAlgorithm}; // We use "Map" for our tile array; rename to FovMap
use rand::prelude::*;

// Tutorial: https://tomassedovic.github.io/roguelike-tutorial/index.html
// tcod-rs library docs: https://tomassedovic.github.io/tcod-rs/tcod/index.html
// libtcod: https://github.com/libtcod/libtcod
//          http://roguecentral.org/doryen/data/libtcod/doc/1.5.1/index2.html

/******************************************************************************
Consts and types
*******************************************************************************/

const VERSION: &str = "0.0.0";
// Window size/resolution
const SCREEN_WIDTH: i32 = 80;
const SCREEN_HEIGHT: i32 = 50;

const LIMIT_FPS: i32 = 60;

// Map size
const MAP_WIDTH: i32 = 80;
const MAP_HEIGHT: i32 = 43;

const ROOM_MAX_SIZE: i32 = 10;
const ROOM_MIN_SIZE: i32 = 6;
const MAX_ROOMS: i32 = 30;

const COLOR_DARK_WALL: Color = Color { r: 0, g: 6, b: 13 };
const COLOR_LIGHT_WALL: Color = Color { r: 107, g: 0, b: 143 };
const COLOR_DARK_GROUND: Color = Color { r: 30, g: 38, b: 48 };
const COLOR_LIGHT_GROUND: Color = Color { r: 131, g: 142, b: 181 };

// http://www.roguebasin.com/index.php?title=Comparative_study_of_field_of_view_algorithms_for_2D_grid_based_worlds
const FOV_ALGO: FovAlgorithm = FovAlgorithm::Basic;
const FOV_LIGHT_WALLS: bool = true;
const TORCH_RADIUS: i32 = 10;

// XP and levels
const LEVEL_XP_RANGE: [i32; 10] = [60, 138, 290, 550, 940, 1498, 2247, 3145, 4403, 5724];

const HEAL_AMOUNT: i32 = 40;
const ENE_AGGR_RANGE: i32 = 5;
const ENE_AGGR_DAMAGE: i32 = 40;
const CONFUSE_RANGE: i32 = 8;
const CONFUSE_NUM_TURNS: i32 = 10;
const NEG_ENE_GREN_RADIUS: i32 = 3;
const NEG_ENE_GREN_DAMAGE: i32 = 25;

// Player will always be the first object
const PLAYER: usize = 0;

// GUI
const BAR_WIDTH: i32 = 20;
const PANEL_HEIGHT: i32 = 7;
const PANEL_Y: i32 = SCREEN_HEIGHT - PANEL_HEIGHT;
const MSG_X: i32 = BAR_WIDTH + 2;
const MSG_WIDTH: i32 = SCREEN_WIDTH - BAR_WIDTH - 2;
const MSG_HEIGHT: usize = PANEL_HEIGHT as usize - 1;
const INVENTORY_WIDTH: i32 = 50;
const LEVEL_SCREEN_WIDTH: i32 = 40;
const STATS_SCREEN_WIDTH: i32 = 30;

type Map = Vec<Vec<Tile>>; // 2D map of tiles
type Messages = Vec<(String, Color)>;

/******************************************************************************
Structs, enums, and traits
*******************************************************************************/

// libtcod bits
struct Tcod {
    root: Root,
    con: Offscreen,
    panel: Offscreen,
    fov: FovMap,
    mouse: Mouse,
}

/// A map tile
#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
struct Tile {
    blocked: bool,
    block_sight: bool,
    explored: bool,
}
impl Tile {
    pub fn new_empty() -> Self {
        Tile{blocked: false, block_sight: false, explored: false}
    }

    pub fn new_wall() -> Self {
        Tile{blocked: true, block_sight: true, explored: false}
    }
}

/// Rectangular area, used for making rooms
#[derive(Clone, Copy, Debug)]
struct Rect {
    x1: i32,
    y1: i32,
    x2: i32,
    y2: i32,
}
impl Rect {
    pub fn new(x: i32, y: i32, w: i32, h: i32) -> Self {
        Rect { x1: x, y1: y, x2: x + w, y2: y + h}
    }

    pub fn center(&self) -> (i32, i32) {
        let center_x = (self.x1 + self.x2) / 2;
        let center_y = (self.y1 + self.y2) / 2;
        (center_x, center_y)
    }

    pub fn intersects_with(&self, other: &Rect) -> bool {
        (self.x1 <= other.x2) && (self.x2 >= other.x1) &&
            (self.y1 <= other.y2) && (self.y2 >= other.y1)
    }
}

/// Generic represention of the player, NPCs, items, certain terrain fixtures, etc.
/// An Object is always represented by an on-screen char.
#[derive(Debug, Serialize, Deserialize)]
struct Object {
    x: i32,
    y: i32,
    char: char,
    color: Color,
    name: String,
    blocks: bool,
    always_visible: bool,
    alive: bool,
    // Components
    fighter: Option<Fighter>,
    ai: Option<Ai>,
    item: Option<Item>,
    equipment: Option<Equipment>,
}
impl Object {
    pub fn new(x: i32, y: i32, char: char, color: Color, name: &str, blocks: bool) -> Self {
        Object {
            x: x,
            y: y,
            char: char,
            color: color,
            name: name.into(),
            blocks: blocks,
            always_visible: false,
            alive: false,
            // Components
            fighter: None,
            ai: None,
            item: None,
            equipment: None,
        }
    }

    pub fn pos(&self) -> (i32, i32) {
        (self.x, self.y)
    }

    pub fn set_pos(&mut self, x: i32, y: i32) {
        self.x = x;
        self.y = y;
    }

    // move_by used to be here, but double-borrows forced us to take it outside...

    /// Set the color of and draw the character that represents this object at its position
    pub fn draw(&self, con: &mut Console) {
        con.set_default_foreground(self.color);
        con.put_char(self.x, self.y, self.char, BackgroundFlag::None);
    }

    /// Erase the character that represents this object
    pub fn clear(&self, con: &mut Console) {
        con.put_char(self.x, self.y, ' ', BackgroundFlag::None);
    }

    /// Return distance to another object
    // TODO: Use Chebyshev
    pub fn distance_to(&self, other: &Object) -> f32 {
        let dx = other.x - self.x;
        let dy = other.y - self.y;
        ((dx.pow(2) + dy.pow(2)) as f32).sqrt() // Euclidean
    }

    // Return distance to some coords
    pub fn distance(&self, x: i32, y: i32) -> f32 {
        (((self.x - x).pow(2) + (self.y - y).pow(2)) as f32).sqrt() // Euclidean
    }

    pub fn get_max_hp(&self, game: &Game) -> i32 {
        let base = self.fighter.map_or(0, |f| f.base_max_hp);
        let buff = self.get_all_equipped(game).iter().fold(0, |acc, e| acc + e.hp_buff);
        // TODO: Add support for consumables!
        base + buff
    }

    pub fn get_power(&self, game: &Game) -> i32 {
        let base = self.fighter.map_or(0, |f| f.base_power);
        // Add the power buffs of all equipped items
        let buff = self.get_all_equipped(game).iter().fold(0, |acc, e| acc + e.power_buff);
        base + buff
    }

    pub fn get_defense(&self, game: &Game) -> i32 {
        let base = self.fighter.map_or(0, |f| f.base_defense);
        let buff = self.get_all_equipped(game).iter().fold(0, |acc, e| acc + e.defense_buff);
        base + buff
    }

    /// Applies damage if this object has Fighter and returns kill XP in an option
    /// on this object's death
    pub fn take_damage(&mut self, damage: i32, game: &mut Game) -> Option<i32> {
        // Apply damage if possible
        if let Some(fighter) = self.fighter.as_mut() { // Mutable borrow of &mut self!
            if damage > 0 {
                fighter.hp -= damage;
            }
        }
        // Check for death to call death function and return kill XP
        if let Some(fighter) = self.fighter { // This would be a second mutable borrow, so new block.
            if fighter.hp <= 0 {
                self.alive = false;
                fighter.on_death.callback(self, game);
                return Some(fighter.xp);
            }
        }
        None
    }

    pub fn attack(&mut self, target: &mut Object, game: &mut Game) {
        // Simple formula for attack damage
        let damage = self.get_power(game) - target.get_defense(game);
        if damage > 0 {
            game.log.add(format!("{} attacks {} for {} HP.", self.name, target.name, damage),
                         colors::WHITE);
            if let Some(xp) = target.take_damage(damage, game) {
                // Award XP to slayer
                self.fighter.as_mut().unwrap().xp += xp; // Unwrap self's Fighter option
                // If self lacks Fighter, this will induce a crash, but that should be okay
                // since non-Fighter entities shouldn't be doing damage and killing things.
            }
        } else {
            game.log.add(format!("{} attacks {} but it has no effect.", self.name, target.name),
                    colors::WHITE);
        }
    }

    // TODO: Add use_item here, with a signature that also takes this object's ID for XP awards
    // when it gets kills with items, abilities, etc.

    // Heal up to max_hp by given amount
    pub fn heal(&mut self, amount: i32, game: &Game) {
        let max_hp = self.get_max_hp(game);

        if let Some(fighter) = self.fighter.as_mut() {
            fighter.hp += amount;
            if fighter.hp > max_hp {
                fighter.hp = max_hp;
            }
        }
    }

    pub fn equip(&mut self, log: &mut Vec<(String, Color)>) {
        if self.item.is_none() {
            log.add("You can only equip items.", colors::RED);
        } else {
            if let Some(eq) = self.equipment.as_mut() {
                if ! eq.equipped {
                    eq.equipped = true;
                    log.add(format!("You equip {}.", self.name), colors::LIGHT_GREEN);
                } // If item was already equipped, do nothing...
            } else {
                log.add("That item can't be equipped.", colors::RED);
            }
        }
    }

    pub fn unequip(&mut self, log: &mut Vec<(String, Color)>) {
        if self.item.is_none() {
            log.add("You can only unequip items.", colors::RED);
        } else {
            if let Some(eq) = self.equipment.as_mut() {
                if eq.equipped {
                    eq.equipped = false;
                    log.add(format!("You unequip {}.", self.name), colors::LIGHT_YELLOW);
                } // If item wasn't equipped, do nothing...
            } else {
                log.add("That item can't be unequipped.", colors::RED);
            }
        }
    }

    pub fn get_all_equipped(&self, game: &Game) -> Vec<Equipment> {
        if self.name == "player" { // Right now, the player is the only object that can equip.
            game.inv.iter()
                .filter(|item| {
                    item.equipment.map_or(false, |e| e.equipped)
                })
                .map(|item| item.equipment.unwrap())
                .collect()
        } else {
            vec![]
        }
    }
}

// The game and its vital organs
// (Except for objects, which is tentatively kept separate to avoid borrow checker issues)
#[derive(Serialize, Deserialize)] // https://docs.serde.rs/serde/trait.Serialize.html
struct Game {
    map: Map,
    log: Messages,
    inv: Vec<Object>,
    dungeon_level: u32,
}

// We'll use composition (in the form of components) rather than inheritance to determine
// object behavior. Component structs containing a behavior or functionality will be added to
// objects that should have that behavior.
// E.g., since the player should be able to fight, its object will contain
// a Fighter component. Then, combat-related functions affect the player object accordingly: a
// projectile that strikes the player will decrement the HP in its Fighter instance.
// Conversely, say a stairs object doesn't have a Fighter instance. Then, the projectile_hit()
// function would do nothing to the stairs object.
//
// See more: https://en.wikipedia.org/wiki/Entity%E2%80%93component%E2%80%93system

// Able to attack or be attacked, die meaningfully, and earn XP
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
struct Fighter {
    base_max_hp: i32,
    hp: i32,
    base_defense: i32,
    base_power: i32,

    xp: i32,
    level: i32,

    on_death: DeathCallback, // Death behavior
}

// Things with a brain... or not
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
enum Ai {
    Basic,
    Confused{previous_ai: Box<Ai>, num_turns: i32},
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum PlayerAction {
    TookTurn,
    DidntTakeTurn,
    Exit,
}

// Item "categories" associated with an effect function in use_item()
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
enum Item {
    // Consumables
    Heal,
    Aggregate,
    Confuse,
    NegativeEnergyGrenade,
    // Equipment
    Sword,
    Shield,
}

// Able to be equipped
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
struct Equipment {
    slot: Slot,
    equipped: bool,
    hp_buff: i32,
    power_buff: i32,
    defense_buff: i32,
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
enum Slot {
    LeftHand,
    RightHand,
    Head,
}
impl std::fmt::Display for Slot {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {
            Slot::LeftHand => write!(f, "in left hand"),
            Slot::RightHand => write!(f, "in right hand"),
            Slot::Head => write!(f, "on head"),
        }
    }
}

// On death, monsters drop loot and leave corpses, the player loses the game, etc.
// These enums are used by component structs to know what function to call on an object's death.
#[derive(Clone, Copy, Debug, PartialEq, Serialize, Deserialize)]
enum DeathCallback {
    Player,
    Monster,
}
impl DeathCallback {
    fn callback(self, object: &mut Object, game: &mut Game) {
        let callback: fn(&mut Object, &mut Game) = match self {
            DeathCallback::Player => player_death,
            DeathCallback::Monster => monster_death,
        };
        callback(object, game);
    }
}

// Item after-use states, determining item consumption
enum UseResult {
    UsedUp,
    UsedAndKept,
    Cancelled,
}

// Trait added to Vec to be able to do game.log.add("You died", colors::RED);
trait MessageLog {
    fn add<T: Into<String>>(&mut self, message: T, color: Color);
}
impl MessageLog for Vec<(String, Color)> {
    /// Adds a message to the log, taking anything T that implements Into trait for String (i.e.,
    /// can be converted to String)
    fn add<T: Into<String>>(&mut self, message: T, color: Color) {
        // If buffer full, remove first message to make room
        if self.len() == MSG_HEIGHT {
            self.remove(0);
        }
        self.push((message.into(), color));
    }
}

/******************************************************************************
UI functions
*******************************************************************************/

/// Renders a colored stat bar (representing HP, XP, etc.)
fn render_bar(panel: &mut Offscreen,
              x: i32,
              y: i32,
              total_width: i32,
              name: &str,
              value: i32,
              max: i32,
              bar_color: Color,
              back_color: Color) {
        // Make a variable meter bar
        let bar_width = (value as f32 / max as f32 * total_width as f32) as i32;
        // Render background first
        panel.set_default_background(back_color);
        panel.rect(x, y, total_width, 1, false, BackgroundFlag::Screen);
        // Then bar
        panel.set_default_background(bar_color);
        if bar_width > 0 {
            panel.rect(x, y, bar_width, 1, false, BackgroundFlag::Screen);
        }

        // Floating, centered text for extra clarity
        panel.set_default_foreground(colors::WHITE);
        panel.print_ex(x + total_width / 2, y, BackgroundFlag::None, TextAlignment::Center,
        &format!("{}: {}/{}", name, value, max));
}

/// Returns string with the names of all objects under the mouse cursor
fn get_names_under_mouse(mouse: Mouse, objects: &[Object], fov_map: &FovMap) -> String {
    let (x, y) = (mouse.cx as i32, mouse.cy as i32);

    // Create list of object names at cursor tile and in FOV
    let names = objects
        .iter()
        .filter(|obj| {obj.pos() == (x, y) && fov_map.is_in_fov(obj.x, obj.y)})
        .map(|obj| obj.name.clone())
        .collect::<Vec<_>>();

    names.join(", ")
}

/// Returns position of a tile left-clicked in the player's FOV (optionally, within a range)
/// or (None, None) if cancelled by right-click or ESC
/// Used for mouse-targeting
fn target_tile(tcod: &mut Tcod,
               objects: &[Object],
               game: &mut Game,
               max_range: Option<f32>) -> Option<(i32, i32)> {
    use tcod::input::KeyCode::Escape;

    loop {
        // Render the screen.
        // This takes away the inventory panel and enters mouse targeting mode.
        tcod.root.flush();

        let event = input::check_for_event(input::KEY_PRESS | input::MOUSE).map(|e| e.1);
        let mut key = None;
        match event {
            Some(Event::Mouse(m)) => tcod.mouse = m,
            Some(Event::Key(k)) => key = Some(k),
            None => {}
        }
        render_all(tcod, objects, game, false);

        let (x, y) = (tcod.mouse.cx as i32, tcod.mouse.cy as i32);

        // Accept target if clicked a tile in FOV &&, if a range was specified (for AoE effects),
        // if it's in that range
        let in_fov = (x < MAP_WIDTH) && (y < MAP_HEIGHT) && tcod.fov.is_in_fov(x, y);
        let in_range = max_range.map_or(true, |range| objects[PLAYER].distance(x, y) <= range);
        if tcod.mouse.lbutton_pressed && in_fov && in_range {
            return Some((x, y))
        }

        // Stop when user presses ESC or right-clicks
        let escape = key.map_or(false, |k| k.code == Escape);
        if tcod.mouse.rbutton_pressed || escape {
            return None
        }
    }
}

/// Using target_tile(), returns a monster object on a clicked tile inside FOV up to a range or
/// None if cancelled by right-click or ESC
fn target_monster(tcod: &mut Tcod,
                  objects: &[Object],
                  game: &mut Game,
                  max_range: Option<f32>) -> Option<usize> {
    loop {
        match target_tile(tcod, objects, game, max_range) {
            Some((x, y)) => { // Received specified tile from target_tile()
                // Return monster on that tile, if any
                for (id, obj) in objects.iter().enumerate() {
                    if obj.pos() == (x, y) && obj.fighter.is_some() && id != PLAYER {
                        return Some(id)
                    }
                }
            }
            None => return None
        }
    }
}

/// Menu screen where the user can select items by their corresponding letter keys
/// header - Window title or description (e.g., "Inventory", "Choose an item.")
/// options - List of items
/// width - of the window (height is implicit as it depends on header height and number of options)
/// root - Main console
fn menu<T: AsRef<str>>(header: &str, options: &[T], width: i32, root: &mut Root) -> Option<usize> {
    assert!(options.len() <= 26, "Cannot have a menu with more than 26 options (for now)!");
    // Calc total height of header (after auto-wrap) and one line per option
    let header_height = if header.is_empty() {
        0
    } else {
        root.get_height_rect(0, 0, width, SCREEN_HEIGHT, header)
    };
    let total_height = options.len() as i32 + header_height;

    // Draw on an off-screen console that acts as the menu window
    let mut window = Offscreen::new(width, total_height);

    // Print header with auto-wrap
    window.set_default_foreground(colors::WHITE);
    window.print_rect_ex(0, 0, width, total_height, BackgroundFlag::None, TextAlignment::Left, header);

    // Print options, using Iterator.enumerate() to get the index of each item and using that
    // to calc the item's y coordinate and selection letter
    for (index, option_text) in options.iter().enumerate() {
        // Start at 'a' byte value (must be u8) to char
        let menu_letter = (b'a' + index as u8) as char;
        let text = format!("({}) {}", menu_letter, option_text.as_ref());
        window.print_ex(0, header_height + index as i32,
                        BackgroundFlag::None, TextAlignment::Left, text);
    }

    // Blit new window onto root
    let x = SCREEN_WIDTH / 2 - width / 2;
    let y = SCREEN_HEIGHT / 2 - total_height / 2;
    tcod::console::blit(&mut window, (0, 0), (width, total_height), root, (x, y), 1.0, 0.7);
    // Note: 1.0, 0.7 params at the end specify foreground, background opaqueness! Fancy.

    // Present frame
    root.flush();

    // Freeze rendering on the frame with inventory window by waiting for key inpit
    let key = root.wait_for_keypress(true);
    // Convert input ASCII code to an index; if it corresponds to an option index, return it.
    if key.printable.is_alphabetic() {
        let index = key.printable.to_ascii_lowercase() as usize - 'a' as usize;
        if index < options.len() {
            Some(index)
        } else {
            None
        }
    } else {
        None
    }
}

/// Inventory screen utilizing menu()
fn inventory_menu(inventory: &[Object], header: &str, root: &mut Root) -> Option<usize> {
    // Show a meny with each inventory item as an option
    let options = if inventory.len() == 0 {
        vec!["Inventory is empty.".into()]
    } else {
        inventory.iter().map(|item| {
            // Show additional info for equipped items
            match item.equipment {
                Some(eq) if eq.equipped => {
                    format!("{} ({})", item.name, eq.slot)
                }
                _ => item.name.clone()
            }
        }).collect()
    };

    let inventory_index = menu(header, &options, INVENTORY_WIDTH, root);

    // If an item was picked, return it.
    if inventory.len() > 0 {
        inventory_index
    } else {
        None
    }
}

/******************************************************************************
Object-based functions
*******************************************************************************/

/// Move an object by the given amount if the destination isn't blocked.
fn move_by(id: usize,
           dx: i32,
           dy: i32,
           objects: &mut [Object],
           game: &Game) {
    let (x, y) = objects[id].pos();
    if !is_blocked(x + dx, y + dy, objects, &game.map) {
        objects[id].set_pos(x + dx, y + dy);
    }
}

/// Moves the player to an unblocked tile or attacks a hostile actor on that tile
fn player_move_or_attack(dx: i32,
                         dy: i32,
                         objects: &mut [Object],
                         game: &mut Game) {
    // Coords the player is moving to or attacking
    let x = objects[PLAYER].x + dx;
    let y = objects[PLAYER].y + dy;

    // Try to find attackable object there
    let target_id = objects.iter().position(|object| {
        object.fighter.is_some() && object.pos() == (x, y)
    });

    // Attack if target found; otherwise, move
    match target_id {
        Some(target_id) => {
            let (player, target) = mut_two(PLAYER, target_id, objects);
            player.attack(target, game);
        }
        None => {
            move_by(PLAYER, dx, dy, objects, game);
        }
    }
}

/// Move one square towards a target
fn move_towards(id: usize, target_x: i32, target_y: i32, objects: &mut [Object], game: &mut Game) {
    // Vector and distance from objects[id] to target
    // Note: for diagonal tiles to also be a distance of 1 (as they should be), use Chebyshev,
    // not Euclidean or Manhattan, distance: D = max(|x2 - x1|, |y2 - y1|)
    // https://theory.stanford.edu/~amitp/GameProgramming/Heuristics.html#diagonal-distance
    let dx = target_x - objects[id].x;
    let dy = target_y - objects[id].y;
    let d = ((dx.pow(2) + dy.pow(2)) as f32).sqrt();
    // normalize it to length 1 (preserving direction), then round it and
    // convert to integer so the movement is restricted to the map grid
    let dx = (dx as f32 / d).round() as i32;
    let dy = (dy as f32 / d).round() as i32;
    move_by(id, dx, dy, objects, game);
}

/// Return closest object posessing the Fighter and Ai components
fn closest_monster(max_range: i32, objects: &mut [Object], tcod: &Tcod) -> Option<usize> {
    let mut closest_enemy = None;
    let mut closest_dist = (max_range + 1) as f32; // Start with slightly more than max range

    for (id, object) in objects.iter().enumerate() {
        if (id != PLAYER) && object.fighter.is_some() && object.ai.is_some() &&
            tcod.fov.is_in_fov(object.x, object.y) { // In FOV so spell can't work through walls
            let dist = objects[PLAYER].distance_to(object);

            if dist < closest_dist {
                closest_enemy = Some(id);
                closest_dist = dist;
            }
        }
    }
    closest_enemy
}

/// Mutably borrow two separate elements from the given slice (without this, compiler would
/// complain that we're mutably borrowing the items' container twice)
/// Panics when the indexes are equal or out of bounds
fn mut_two<T>(first_index: usize, second_index: usize, items: &mut [T]) -> (&mut T, &mut T) {
    assert!(first_index != second_index);
    let split_at_index = cmp::max(first_index, second_index);
    let (first_slice, second_slice) = items.split_at_mut(split_at_index);
    if first_index < second_index {
        (&mut first_slice[first_index], &mut second_slice[0])
    } else {
        (&mut second_slice[0], &mut first_slice[second_index])
    }
}

/// Run assigned AI, associating an object's Ai enum type with an AI function and executing that
/// function
fn ai_take_turn(monster_id: usize, fov_map: &FovMap, objects: &mut [Object], game: &mut Game) {
    // take() takes value out of Option, ai, leaving a None in its place
    if let Some(ai) = objects[monster_id].ai.take() {
        let new_ai = match ai {
            Ai::Basic => ai_basic(monster_id, fov_map, objects, game),
            Ai::Confused{previous_ai, num_turns} => ai_confused(monster_id,
                                                                objects,
                                                                game,
                                                                previous_ai,
                                                                num_turns)
        };
        objects[monster_id].ai = Some(new_ai);
    }
}

// Basic melee hostile behavior: if you see a monster, it can see you, and it'll approach until
// in melee range and attack.
fn ai_basic(monster_id: usize,
            fov_map: &FovMap,
            objects: &mut [Object],
            game: &mut Game) -> Ai {
    let (monster_x, monster_y) = objects[monster_id].pos();
    if fov_map.is_in_fov(monster_x, monster_y) {
        if objects[monster_id].distance_to(&objects[PLAYER]) >= 2.0 { // Out of melee range
            // Move towards player
            let (player_x, player_y) = objects[PLAYER].pos();
            move_towards(monster_id, player_x, player_y, objects, game);
        } else if objects[PLAYER].fighter.map_or(false, |f| f.hp > 0) { // In melee range, player alive
            // Attack
            let (monster, player) = mut_two(monster_id, PLAYER, objects);
            monster.attack(player, game);
        }
    }
    Ai::Basic
}

// Confused behavior: deliriously wander around in random directions for num_turns turns
fn ai_confused(monster_id: usize,
               objects: &mut [Object],
               game: &mut Game,
               previous_ai: Box<Ai>,
               num_turns: i32) -> Ai {
    if num_turns >= 0 { // Still confused...
        move_by(monster_id,
                rand::thread_rng().gen_range(-1, 2), // Weird numbers due to Euclidean distances...
                rand::thread_rng().gen_range(-1, 2),
                objects,
                game);
        // ...but one turn closer to sanity.
        Ai::Confused{previous_ai: previous_ai, num_turns: num_turns - 1}
    } else { // No longer confused!
        game.log.add(format!("{} is no longer confused!", objects[monster_id].name), colors::RED);
        *previous_ai // Restore previous AI
    }
}

fn player_death(player: &mut Object, game: &mut Game) {
    // Game over!
    game.log.add("You died!", colors::RED);

    // Turn player into corpse
    player.char = '%';
    player.color = colors::DARK_RED;
}

fn monster_death(monster: &mut Object, game: &mut Game) {
    // Turn monster into corpse
    game.log.add(format!("{} dies.\nYou gain {} XP.",
                         monster.name,
                         monster.fighter.unwrap().xp),
                         colors::ORANGE);
    monster.char = '%';
    monster.color = colors::DARK_RED;
    monster.blocks = false;
    monster.fighter = None;
    monster.ai = None;
    monster.name = format!("{} corpse", monster.name);
}

// Level up the player
fn level_up(tcod: &mut Tcod, objects: &mut [Object], game: &mut Game) {
    let pf = objects[PLAYER].fighter.as_mut().unwrap(); // Crash if player lacks Fighter
    // Rust noob note: Must use as_mut(), otherwise, a new Fighter is copied into this scope.
    // We do that frequently throughout this code when we're only reading from Fighters.
    // With foo: Option<T>, foo.as_mut() = Option<&mut T>, which .unwrap()s to a &mut T.

    if pf.xp >= LEVEL_XP_RANGE[pf.level as usize] {
        pf.level += 1; // Ding!
        game.log.add(format!("You've reached level {}!", pf.level), colors::YELLOW);
        // Increase stats
        let mut choice = None;

        while choice.is_none() { // Keep asking until a choice is made
            choice = menu("Choose a stat to raise:\n",
                          &[format!("+20 HP (to {} HP)", pf.base_max_hp + 20),
                            format!("+1 power (to {})", pf.base_power + 1),
                            format!("+1 defense (to {})", pf.base_defense + 1)],
                          LEVEL_SCREEN_WIDTH,
                          &mut tcod.root);
        };
        match choice.unwrap() {
            0 => {
                pf.base_max_hp += 20;
                pf.hp += 20;
            }
            1 => {
                pf.base_power += 1;
            }
            2 => {
                pf.base_defense += 1;
            }
            _ => unreachable!(),
        }
    }
}

fn pick_item_up(object_id: usize, objects: &mut Vec<Object>, game: &mut Game) {
    if game.inv.len() >= 26 { // Arbitrary limit to map items to letter keys for selection
        game.log.add(format!("Your inventory is full."),
                     colors::WHITE);
    } else {
        let item = objects.swap_remove(object_id);
        game.log.add(format!("Picked up {}.", item.name),
                colors::GREEN);
        game.inv.push(item);
    }
}

fn drop_item(inventory_id: usize,
             objects: &mut Vec<Object>,
             game: &mut Game) {
    let mut item = game.inv.remove(inventory_id);
    if item.equipment.is_some() { // If equippable, try to unequip in case it was equipped.
        item.unequip(&mut game.log);
    }
    item.set_pos(objects[PLAYER].x, objects[PLAYER].y);
    game.log.add(format!("Dropped {}.", item.name),
                 colors::YELLOW);
    objects.push(item);
}

/// Uses an inventory item, associating its Item enum type with an effect function and executing
/// that function
fn use_item(tcod: &mut Tcod,
            inventory_id: usize,
            objects: &mut [Object],
            game: &mut Game) {
    // Attempt to get item
    if let Some(item) = game.inv[inventory_id].item {
        // Get item's effect function
        // Note: because the different match arms return different functions, Rust treats them as
        // returning different types, which it doesn't allow.
        // To fix, we have to annotate on_use() to specify that all the different functions
        // satisfy a uniform signature.
        // This means some of the functions have params they may not need.
        // These are prefixed with a '_' to disable unused warnings:
        // https://stackoverflow.com/questions/48361537/why-do-underscore-prefixed-variables-exist
        let on_use: fn(&mut Tcod, usize, &mut [Object], &mut Game) -> UseResult = match item {
            Item::Heal => cast_heal,
            Item::Aggregate => cast_ene_aggr,
            Item::Confuse => cast_confuse,
            Item::NegativeEnergyGrenade => cast_neg_ene_gren,

            Item::Sword => toggle_equipment,
            Item::Shield => toggle_equipment,
        };

        // If we have an item, match its effect type
        match on_use(tcod, inventory_id, objects, game) {
            UseResult::UsedUp => { // Destroy after use (unless it was cancelled)
                game.inv.remove(inventory_id);
            }
            UseResult::UsedAndKept => {}
            UseResult::Cancelled => {
                game.log.add("Cancelled", colors::WHITE);
            }
        }
    } else {
        game.log.add(format!("{} cannot be used.", game.inv[inventory_id].name),
                     colors::WHITE);
    }
}

/// Player heal item effect function, associated with Item::Heal in use_item()
/// It just carries out the effect logic, including calling the player object's heal function,
/// and returns an appropriate UseResult for the responsible item.
fn cast_heal(_tcod: &mut Tcod,
             _inventory_id: usize,
             objects: &mut [Object],
             game: &mut Game) -> UseResult {
    let player = &mut objects[PLAYER];

    if let Some(fighter) = player.fighter {
        if fighter.hp == player.get_max_hp(game) {
            game.log.add("You're already at full HP!", colors::RED);
            return UseResult::Cancelled;
        }
        game.log.add("Your wounds begin knitting themselves shut.", colors::LIGHT_VIOLET);
        // Call player object heal function with HEAL_AMOUNT associated with the item type of
        // objects[inventory_id]
        player.heal(HEAL_AMOUNT, game);
        return UseResult::UsedUp;
    }
    UseResult::Cancelled
}

// Find closest enemy inside some range and aggregate it!
fn cast_ene_aggr(tcod: &mut Tcod,
                  _inventory_id: usize,
                  objects: &mut [Object],
                  game: &mut Game) -> UseResult {
    let monster_id = closest_monster(ENE_AGGR_RANGE, objects, tcod);
    if let Some(monster_id) = monster_id {
        game.log.add(format!("{} takes {} damage from being aggregated!",
                             objects[monster_id].name, ENE_AGGR_DAMAGE),
                     colors::LIGHT_BLUE);
        if let Some(xp) = objects[monster_id].take_damage(ENE_AGGR_DAMAGE, game) {
            // We're assuming the player used this, so award them XP for any kills.
            objects[PLAYER].fighter.as_mut().unwrap().xp += xp;
        }
        UseResult::UsedUp
    } else {
        game.log.add("No viable target is close enough.", colors::RED);
        UseResult::Cancelled
    }
}

// Prompt user for a target monster to disrupt
fn cast_confuse(tcod: &mut Tcod,
                _inventory_id: usize,
                objects: &mut [Object],
                game: &mut Game) -> UseResult {
    game.log.add("Left-click an enemy to disrupt it (right-click or ESC to cancel).",
                 colors::LIGHT_CYAN);
    let monster_id = target_monster(tcod, objects, game, Some(CONFUSE_RANGE as f32));
    if let Some(monster_id) = monster_id {
        let old_ai = objects[monster_id].ai.take().unwrap_or(Ai::Basic);
        // Temporarily replace monster's AI with Ai::Disrupted
        objects[monster_id].ai = Some(
            Ai::Confused {
            previous_ai: Box::new(old_ai),
            num_turns: CONFUSE_NUM_TURNS,
            }
        );
        game.log.add(format!("{} is temporarily confused!",
                     objects[monster_id].name), colors::LIGHT_GREEN);
        UseResult::UsedUp
    } else {
        game.log.add("No viable target is close enough.",
                colors::RED);
        UseResult::Cancelled
    }
}

// Prompt user for a target tile to throw a negaitve energy grenade at
fn cast_neg_ene_gren(tcod: &mut Tcod,
                 _inventory_id: usize,
                 objects: &mut [Object],
                 game: &mut Game) -> UseResult {
    game.log.add("Left-click a tile to throw grenade at (right-click or ESC to cancel).",
            colors::LIGHT_CYAN);
    let (x, y) = match target_tile(tcod, objects, game, None) {
        Some(tile_pos) => tile_pos,
        None => return UseResult::Cancelled,
    };
    game.log.add(format!("A burst of yin followed by its yang echo violently rearrange nearby fields!"),
            colors::ORANGE);
    let mut xp_accumulated = 0; // Can't award player on the fly INSIDE the for loop since our
                                // objects reference is either moved in the loop header
                                //      for obj in objects {...
                                // or borrowed mutably in the loop header
                                //      for obj in objects.iter_mut() {...
                                // and then borrowed by the xp addition.
                                // So we need to add all the XP outside of the loop.
    for obj in objects.iter_mut() { // Damage objects in grenade's AoE
        if obj.distance(x, y) <= NEG_ENE_GREN_RADIUS as f32 && obj.fighter.is_some() {
            game.log.add(format!("{} takes {} damage.", obj.name, NEG_ENE_GREN_DAMAGE),
                         colors::WHITE);
            if let Some(xp) = obj.take_damage(NEG_ENE_GREN_DAMAGE, game) {
                xp_accumulated +=  xp;
            }
        }
    }
    objects[PLAYER].fighter.as_mut().unwrap().xp += xp_accumulated;
    UseResult::UsedUp
}

fn get_equipped_in_slot(slot: Slot, inv: &[Object]) -> Option<usize> {
    for (inv_id, item) in inv.iter().enumerate() {
        if item.equipment.as_ref().map_or(false, |e| e.equipped && e.slot == slot) {
            return Some(inv_id)
        }
    }
    None
}

fn toggle_equipment(_tcod: &mut Tcod,
                    inventory_id: usize,
                    _objects: &mut [Object],
                    game: &mut Game) -> UseResult {
    let eq = match game.inv[inventory_id].equipment {
        Some(eq) => eq,
        None => return UseResult::Cancelled,
    };
    if eq.equipped {
        game.inv[inventory_id].unequip(&mut game.log);
    } else {
        // Unequip whatever, if anything, is already in the slot
        if let Some(prev_eq) = get_equipped_in_slot(eq.slot, &game.inv) {
            game.inv[prev_eq].unequip(&mut game.log);
        }
        // Then equip the item this was called on
        game.inv[inventory_id].equip(&mut game.log);
    }
    UseResult::UsedAndKept
}

/******************************************************************************
Core functions
*******************************************************************************/

/// Returns if a tile is blocked for movement (occupied by a blocking object)
fn is_blocked(x: i32, y: i32, objects: &[Object], map: &Map) -> bool {
    // First test map tile
    if map[x as usize][y as usize].blocked {
        return true;
    }
    // Then check for blocking objects
    objects.iter().any(|object| {
        object.blocks && object.pos() == (x, y)
    })
}

/// Clears an area of the map to create a room
fn create_room(room: Rect, map: &mut Map) {
    for x in (room.x1 + 1)..room.x2 {
        for y in (room.y1 + 1)..room.y2 {
            map[x as usize][y as usize] = Tile::new_empty();
        }
    }
}

// Clear area to create tunnels
fn create_h_tunnel(x1: i32, x2: i32, y: i32, map: &mut Map) {
    for x in cmp::min(x1, x2)..(cmp::max(x1, x2) + 1) {
        map[x as usize][y as usize] = Tile::new_empty();
    }
}
fn create_v_tunnel(y1: i32, y2: i32, x: i32, map: &mut Map) {
    for y in cmp::min(y1, y2)..(cmp::max(y1, y2) + 1) {
        map[x as usize][y as usize] = Tile::new_empty();
    }
}

// Returns a value associated with a given level, typically representing weighted randomness
// (max number of a certain item or mob for that level, etc.)
// table - array of (level, value) tuples
fn from_dungeon_level(table: &[(u32, u32)], curr_level: u32) -> u32 {
    table.iter()
        .rev() // Since we want the exact or immediately-lesser level association
        .find(|transition| transition.0 <= curr_level)
        .map_or(0, |transition| transition.1)
}

/// Populates a room with objects
fn place_objects(room: Rect, objects: &mut Vec<Object>, map: &Map, dungeon_level: u32) {
    // Spawn random number of monsters per room, increasing with higher dungeon levels
    let max_monsters = from_dungeon_level(&[
        (1, 2),
        (4, 3),
        (6, 5),
    ], dungeon_level);
    let num_monsters = rand::thread_rng().gen_range(0, max_monsters + 1);

    for _ in 0..num_monsters {
        let x = rand::thread_rng().gen_range(room.x1 + 1, room.x2);
        let y = rand::thread_rng().gen_range(room.y1 + 1, room.y2);

        if !is_blocked(x, y, objects, map) {
            // Scale monster chances with level difficulty
            let soliton_chance = from_dungeon_level(&[
                (1, 75), // 75% chance for a soliton... until level 5
                (5, 60),
                (7, 50),
            ], dungeon_level);
            let bion_chance = from_dungeon_level(&[
                (1, 25),
                (5, 40),
                (7, 50),
            ], dungeon_level);

            // Pick a monster randomly according to designated chances
            let monster_table = [
                ("soliton", soliton_chance),
                ("bion", bion_chance),
            ];
            let mut rng = rand::thread_rng();
            let dist = rand::distributions::WeightedIndex::new(monster_table
                .iter()
                .map(|mon| mon.1))
                .unwrap();
            let mut monster = match monster_table[dist.sample(&mut rng)].0 {
                "soliton" => {
                    let mut soliton = Object::new(x, y, 's', colors::DARKER_RED, "soliton", true);
                    soliton.fighter = Some(Fighter{
                        base_max_hp: 10,
                        hp: 10,
                        base_defense: 0,
                        base_power: 3,
                        xp: 5,
                        level: 0,
                        on_death: DeathCallback::Monster,
                    });
                    soliton.ai = Some(Ai::Basic);
                    soliton
                }
                "bion" => {
                    let mut bion = Object::new(x, y, 'b', colors::DARKER_GREEN, "bion", true);
                    bion.fighter = Some(Fighter{
                        base_max_hp: 20,
                        hp: 16,
                        base_defense: 2,
                        base_power: 8,
                        xp: 10,
                        level: 0,
                        on_death: DeathCallback::Monster,
                    });
                    bion.ai = Some(Ai::Basic);
                    bion
                }
                _ => unreachable!(),
            };
            monster.alive = true; // It's alive!!!
            objects.push(monster);
        }
    }

    // Spawn random number of items per room
    let max_items = from_dungeon_level(&[
        (1, 1),
        (4, 2),
    ], dungeon_level);
    let num_items = rand::thread_rng().gen_range(0, max_items + 1);

    // Random item table
    let heal_chances = from_dungeon_level(&[
        // Healing items become slightly more common... bions are tough.
        (1, 70),
        (5, 75),
        (7, 80),
    ], dungeon_level);

    for _ in 0..num_items {
        let x = rand::thread_rng().gen_range(room.x1 + 1, room.x2);
        let y = rand::thread_rng().gen_range(room.y1 + 1, room.y2);

        if !is_blocked(x, y, objects, map) {
            let item_table = &mut [
                (Item::Heal, heal_chances),
                (Item::Aggregate, 10),
                (Item::NegativeEnergyGrenade, 10),
                (Item::Confuse, 10),
                (Item::Sword, 20),
                (Item::Shield, 20),
            ];
            let mut rng = rand::thread_rng();
            let dist = rand::distributions::WeightedIndex::new(item_table
                .iter()
                .map(|it| it.1))
                .unwrap();
            let mut item = match item_table[dist.sample(&mut rng)].0 {
                Item::Heal => {
                    let mut object = Object::new(x, y, '#', colors::FUCHSIA, "antientropic fabric", false);
                    object.item = Some(Item::Heal);
                    object
                }
                Item::Aggregate => {
                    let mut object = Object::new(x, y, '^', colors::LIGHT_YELLOW,
                                                 "aggregator", false);
                    object.item = Some(Item::Aggregate);
                    object
                }
                Item::NegativeEnergyGrenade => {
                    let mut object = Object::new(x, y, '"', colors::LIGHT_YELLOW,
                                                 "negative energy grenade", false);
                    object.item = Some(Item::NegativeEnergyGrenade);
                    object
                }
                Item::Confuse => {
                    let mut object = Object::new(x, y, '"', colors::LIGHT_YELLOW,
                                                 "disruptor", false);
                    object.item = Some(Item::Confuse);
                    object
                }
                Item::Sword => {
                    let mut object = Object::new(x, y, '/', colors::SKY, "sword", false);
                    object.item = Some(Item::Sword);
                    object.equipment = Some(Equipment{ equipped: false,
                                                       slot: Slot::RightHand,
                                                       hp_buff: 0,
                                                       power_buff: 3,
                                                       defense_buff: 0, });
                    object
                }
                Item::Shield => {
                    let mut object = Object::new(x, y, '[', colors::GREY, "shield", false);
                    object.item = Some(Item::Shield);
                    object.equipment = Some(Equipment{ equipped: false,
                                                       slot: Slot::LeftHand,
                                                       hp_buff: 0,
                                                       power_buff: 2,
                                                       defense_buff: 0, });
                    object
                }
            };
            item.always_visible = true;
            objects.push(item);
        }
    }
}

/// Creates a new level
fn make_map(objects: &mut Vec<Object>, dungeon_level: u32) -> Map {
    // Fill level with wall tiles (underground)
    let mut map = vec![vec![Tile::new_wall(); MAP_HEIGHT as usize]; MAP_WIDTH as usize];

    // Remove any old objects except the player
    // (We compare raw pointers to make sure first object is the player object. This is crashes
    // if it's not due to some refactor later.)
    assert_eq!(&objects[PLAYER] as *const _, &objects[0] as *const _);
    objects.truncate(1);

    // Hollow out {MAX_ROOMS} randomly-sized rooms
    let mut rooms: Vec<Rect> = vec![]; // Reference of rooms for tunnel rendering, etc.

    for _ in 0..MAX_ROOMS {
        // Random width, height
        let w = rand::thread_rng().gen_range(ROOM_MIN_SIZE, ROOM_MAX_SIZE + 1);
        let h = rand::thread_rng().gen_range(ROOM_MIN_SIZE, ROOM_MAX_SIZE + 1);
        // Random position without exceeding map bounds
        let x = rand::thread_rng().gen_range(0, MAP_WIDTH - w);
        let y = rand::thread_rng().gen_range(0, MAP_HEIGHT - h);

        let new_room = Rect::new(x, y, w, h);

        // Test for intersection with every other room
        let failed = rooms.iter().any(|other_room| new_room.intersects_with(other_room));

        if !failed {
            create_room(new_room, &mut map);
            let (new_x, new_y) = new_room.center();
            if rooms.is_empty() { // First room, where player starts
                objects[PLAYER].set_pos(new_x, new_y);
            } else {
                // Connect all subsequent rooms to their previous room with a tunnel
                let (prev_x, prev_y) = rooms[rooms.len() - 1].center();

                // Coin flip between hor,vert and vert,hor connection order
                if rand::random() { // hor,vert
                    create_h_tunnel(prev_x, new_x, prev_y, &mut map);
                    create_v_tunnel(prev_y, new_y, new_x, &mut map);
                } else { // vert,hor
                    create_v_tunnel(prev_y, new_y, prev_x, &mut map);
                    create_h_tunnel(prev_x, new_x, new_y, &mut map);
                }
            }
            // Populate every room
            place_objects(new_room, objects, &map, dungeon_level);

            rooms.push(new_room);
        }
    }

    // Put stairs at the center of last room
    let (last_room_x, last_room_y) = rooms[rooms.len() - 1].center();
    let mut stairs = Object::new(last_room_x, last_room_y, '>', colors::WHITE, "stairs down", false);
    stairs.always_visible = true;
    objects.push(stairs);

    map
}

/// Builds a new level and advances the player to it
fn next_level(tcod: &mut Tcod, objects: &mut Vec<Object>, game: &mut Game) {
    game.log.add("You delve deeper...", colors::GREY);

    game.dungeon_level += 1;
    game.map = make_map(objects, game.dungeon_level);
    init_fov(tcod, &game.map);
}


fn render_all(tcod: &mut Tcod, objects: &[Object], game: &mut Game, fov_recompute: bool) {
    if fov_recompute { // Player moved
        let player = &objects[PLAYER];
        tcod.fov.compute_fov(player.x, player.y, TORCH_RADIUS, FOV_LIGHT_WALLS, FOV_ALGO);

        // Draw all map tiles, shading by visibility using FovMap
        for y in 0..MAP_HEIGHT {
            for x in 0..MAP_WIDTH {
                let visible = tcod.fov.is_in_fov(x, y);
                let wall = game.map[x as usize][y as usize].block_sight;
                let color = match (visible, wall) {
                    // Outside FOV
                    (false, true) => COLOR_DARK_WALL,
                    (false, false) => COLOR_DARK_GROUND,
                    // Inside FOV
                    (true, true) => COLOR_LIGHT_WALL,
                    (true, false) => COLOR_LIGHT_GROUND // Ignoring TORCH_RADIUS! Trouble in is_in_fov()?
                };

                let explored = &mut game.map[x as usize][y as usize].explored;
                if visible { // If tile seen, it's been explored.
                    *explored = true;
                }
                if *explored { // If tile explored, render it.
                    tcod.con.set_char_background(x, y, color, BackgroundFlag::Set);
                }
            }
        }
    }

    // Draw all objects
    // Set object render hierarchy (e.g., live actors cover corpses and items), but only bother
    // if they're visible
    let mut to_draw: Vec<_> = objects.iter()
        .filter(|o| {
            tcod.fov.is_in_fov(o.x, o.y) ||
                (o.always_visible &&  game.map[o.x as usize][o.y as usize].explored)
        }).collect();
    // Sort so that non-blocking objects come first
    // Note: Below, |a, b| tacitly means "a is less than b", so:
    //      a.cmp(b) = Ordering::Less
    // which sorts the container in ascending order, and
    //      b.cmp(a) = Ordering::Greater
    // which sorts it in descending order.
    // Also, false < true in bool comparisons.
    to_draw.sort_unstable_by(|o1, o2| { o1.blocks.cmp(&o2.blocks) }); // false < true ascending
    for object in &to_draw {
        object.draw(&mut tcod.con);
    }

    // Blit con onto root console
    // Blit doc: https://tomassedovic.github.io/tcod-rs/tcod/console/fn.blit.html
    blit(&tcod.con, (0, 0), (MAP_WIDTH, MAP_HEIGHT), &mut tcod.root, (0, 0), 1.0, 1.0);

    // Begin rendering GUI panel
    tcod.panel.set_default_background(colors::BLACK);
    tcod.panel.clear();

    // Print all messages, using libtcod's get_height_rect() and print_rect() for line wrapping
    let mut y = MSG_HEIGHT as i32;
    for &(ref msg, color) in game.log.iter().rev() { // .rev() to start with last message
        // Calc height of message to wrap
        let msg_height = tcod.panel.get_height_rect(MSG_X, y, MSG_WIDTH, 0, msg);

        y -= msg_height; // Print message upwards
        if y < 0 { // Out of room; we're printing above the panel (which is invisible anyway),
            break; // so stop.
        }
        tcod.panel.set_default_foreground(color);
        tcod.panel.print_rect(MSG_X, y, MSG_WIDTH, 0, msg);
    }

    // Show player stats & bars
    let hp = objects[PLAYER].fighter.map_or(0, |f| f.hp);
    let max_hp = objects[PLAYER].get_max_hp(game);
    render_bar(&mut tcod.panel, 1, 1, BAR_WIDTH, "HP", hp, max_hp, colors::LIGHT_RED, colors::DARKER_RED);
    // Show location
    tcod.panel.print_ex(1, 3, BackgroundFlag::None, TextAlignment::Left,
                        format!("Dungeon level: {}", game.dungeon_level));

    // display names of objects under the mouse
    tcod.panel.set_default_foreground(colors::LIGHT_GREY);
    tcod.panel.print_ex(1, 0, BackgroundFlag::None, TextAlignment::Left,
                   get_names_under_mouse(tcod.mouse, objects, &tcod.fov));

    // Blit panel contents to root console
    // Docs: https://tomassedovic.github.io/tcod-rs/tcod/console/fn.blit.html
    blit(&tcod.panel, (0, 0), (SCREEN_WIDTH, PANEL_HEIGHT), &mut tcod.root, (0, PANEL_Y), 1.0, 1.0);

    if let Some(fighter) = objects[PLAYER].fighter {
        tcod.root.print_ex(1, SCREEN_HEIGHT - 2, BackgroundFlag::None, TextAlignment::Left,
                        format!("HP: {}/{} ", fighter.hp, objects[PLAYER].get_max_hp(game)));
    }
}

fn msgbox(root: &mut Root, text: &str, width: i32) {
    let options: &[&str] = &[];
    menu(text, options, width, root);
}

/// Carries out keymapped actions and returns their PlayerAction type.
/// In-game which result in taking a turn (e.g., moving) return TookTurn; meta-game actions
/// (e.g., toggling fullscreen) return other PlayerActions.
fn handle_keys(key: Key,
               tcod: &mut Tcod,
               objects: &mut Vec<Object>,
               game: &mut Game) -> PlayerAction {
    use tcod::input::Key;
    use tcod::input::KeyCode::*;

    let player_alive = objects[PLAYER].alive;
    match (key, player_alive) { // Only allow some actions if not game over
        // Meta actions
        (Key { code: Enter, alt: true, .. }, _) => { // Alt+Enter: toggle fullscreen
            let fullscreen = tcod.root.is_fullscreen();
            tcod.root.set_fullscreen(!fullscreen);
            PlayerAction::DidntTakeTurn
        }
        (Key { code: Escape, .. }, _) => PlayerAction::Exit, // Esc: exit

        // Movement
        // Note: ~1 sec movement delay when changing spammed direction seems like
        // industry standard (same effect in Crawl, etc.).
        /* Arrow

     Home ⮝ PgUp
         \|/
        ⮜-.-⮞
         /|\
     End ⮟ PgDown
        */
        (Key { code: Up, .. }, true) => {
            player_move_or_attack(0, -1, objects, game);
            PlayerAction::TookTurn
        }
        (Key { code: Down, .. }, true) => {
            player_move_or_attack(0, 1, objects, game);
            PlayerAction::TookTurn
        }
        (Key { code: Left, .. }, true) => {
            player_move_or_attack(-1, 0, objects, game);
            PlayerAction::TookTurn
        }
        (Key { code: Right, .. }, true) => {
            player_move_or_attack(1, 0, objects, game);
            PlayerAction::TookTurn
        }
        (Key { code: Home, .. }, true) => {
            player_move_or_attack(-1, -1, objects, game);
            PlayerAction::TookTurn
        }
        (Key { code: PageUp, .. }, true) => {
            player_move_or_attack(1, -1, objects, game);
            PlayerAction::TookTurn
        }
        (Key { code: End, .. }, true) => {
            player_move_or_attack(-1, 1, objects, game);
            PlayerAction::TookTurn
        }
        (Key { code: PageDown, .. }, true) => {
            player_move_or_attack(1, 1, objects, game);
            PlayerAction::TookTurn
        }
        (Key { printable: '.', shift: false, .. }, true) => {
            PlayerAction::TookTurn
        }
        /* Numpad

        7 8 9
         \|/
        4-5-6
         /|\
        1 2 3
        */
        (Key { code: NumPad1, .. }, true) => {
            player_move_or_attack(-1, 1, objects, game);
            PlayerAction::TookTurn
        }
        (Key { code: NumPad2, .. }, true) => {
            player_move_or_attack(0, 1, objects, game);
            PlayerAction::TookTurn
        }
        (Key { code: NumPad3, .. }, true) => {
            player_move_or_attack(1, 1, objects, game);
            PlayerAction::TookTurn
        }
        (Key { code: NumPad4, .. }, true) => {
            player_move_or_attack(-1, 0, objects, game);
            PlayerAction::TookTurn
        }
        (Key { code: NumPad5, .. }, true) => {
            PlayerAction::TookTurn
        }
        (Key { code: NumPad6, .. }, true) => {
            player_move_or_attack(1, 0, objects, game);
            PlayerAction::TookTurn
        }
        (Key { code: NumPad7, .. }, true) => {
            player_move_or_attack(-1, -1, objects, game);
            PlayerAction::TookTurn
        }
        (Key { code: NumPad8, .. }, true) => {
            player_move_or_attack(0, -1, objects, game);
            PlayerAction::TookTurn
        }
        (Key { code: NumPad9, .. }, true) => {
            player_move_or_attack(1, -1, objects, game);
            PlayerAction::TookTurn
        }
        /*(Key { code: NumPad0, .. }, true) => {

        }*/
        /*/* WASDQEZC

        q w e
         \|/
        a-.-d
         /|\
        z s c
        */
        (Key { printable: 'w', .. }, true) => {
            player_move_or_attack(0, -1, map, objects, messages);
            PlayerAction::TookTurn
        }
        (Key { printable: 'a', .. }, true) => {
            player_move_or_attack(-1, 0, map, objects, messages);
            PlayerAction::TookTurn
        }
        (Key { printable: 's', .. }, true) => {
            player_move_or_attack(0, 1, map, objects, messages);
            PlayerAction::TookTurn
        }
        (Key { printable: 'd', .. }, true) => {
            player_move_or_attack(1, 0, map, objects, messages);
            PlayerAction::TookTurn
        }
        (Key { printable: 'q', .. }, true) => {
            player_move_or_attack(-1, -1, map, objects, messages);
            PlayerAction::TookTurn
        }
        (Key { printable: 'e', .. }, true) => {
            player_move_or_attack(1, -1, map, objects, messages);
            PlayerAction::TookTurn
        }
        (Key { printable: 'z', .. }, true) => {
            player_move_or_attack(-1, 1, map, objects, messages);
            PlayerAction::TookTurn
        }
        (Key { printable: 'c', .. }, true) => {
            player_move_or_attack(1, 1, map, objects, messages);
            PlayerAction::TookTurn
        }*/
        /*/* Vi

        y k u
         \|/
        h-.-l
         /|\
        b j n
        */
        (Key { printable: 'y', .. }, true) => {
            player_move_or_attack(-1, -1, map, objects, messages);
            PlayerAction::TookTurn
        }
        (Key { printable: 'k', .. }, true) => {
            player_move_or_attack(0, -1, map, objects, messages);
            PlayerAction::TookTurn
        }
        (Key { printable: 'u', .. }, true) => {
            player_move_or_attack(1, -1, map, objects, messages);
            PlayerAction::TookTurn
        }
        (Key { printable: 'h', .. }, true) => {
            player_move_or_attack(-1, 0, map, objects, messages);
            PlayerAction::TookTurn
        }
        (Key { printable: 'l', .. }, true) => {
            player_move_or_attack(1, 0, map, objects, messages);
            PlayerAction::TookTurn
        }
        (Key { printable: 'b', .. }, true) => {
            player_move_or_attack(-1, 1, map, objects, messages);
            PlayerAction::TookTurn
        }
        (Key { printable: 'j', .. }, true) => {
            player_move_or_attack(0, 1, map, objects, messages);
            PlayerAction::TookTurn
        }
        (Key { printable: 'n', .. }, true) => {
            player_move_or_attack(1, 1, map, objects, messages);
            PlayerAction::TookTurn
        }*/
        (Key { printable: '.', shift: true, .. }, true) => { // Go down stairs
            let player_on_stairs = objects.iter().any(|object| {
                object.name == "stairs down" && object.pos() == objects[PLAYER].pos()
            });

            if player_on_stairs {
                next_level(tcod, objects, game);
            }
            PlayerAction::DidntTakeTurn
        }/*
        (Key, {printable: '<', .. }, true) => { // Go up stairs
            let player_on_stairs = objects.iter().any(|object| {
                object.name == "stairs up" && object.pos() == object[PLAYER].pos()
            });

            if player_on_stairs {
                next_level(tcod, objects, game);
            }
            PlayerAction::DidntTakeTurn
        }
        */
        // Actions
        (Key { printable: 'g', .. }, true) => { // Pick up item
            let item_id = objects.iter().position(|object| {
                object.pos() == objects[PLAYER].pos() && object.item.is_some()
            });
            if let Some(item_id) = item_id {
                pick_item_up(item_id, objects, game);
            }
            PlayerAction::DidntTakeTurn
        }
        (Key { printable: 'd', .. }, true) => { // Drop item
            // Show inv; if an item is selected, drop it.
            let inventory_index = inventory_menu(&game.inv,
                                                 "Select an item by pressing its key or any \
                                                 other key to cancel.\n",
                                                 &mut tcod.root);
            if let Some(inventory_index) = inventory_index {
                drop_item(inventory_index, objects, game);
            }
            PlayerAction::DidntTakeTurn
        }
        (Key { printable: 'i', .. }, true) => { // Show inventory
            let inventory_index = inventory_menu(
                &game.inv,
                "Select an item by pressing its key or any other key to cancel.\n",
                &mut tcod.root
            );
            if let Some(inventory_index) = inventory_index {
                use_item(tcod, inventory_index, objects, game);
            }
            PlayerAction::DidntTakeTurn
        }
        (Key { printable: 'c', .. }, true) => { // Show stats
            let player = &objects[PLAYER];
            if let Some(fighter) = player.fighter {
                let msg = format!("
Character stats

HP: {}/{}
Power: {}
Defense: {}
Level: {}
XP: {} ({} to next level)
                ", // Note: format!() is delightfuly literal about its text formatting!
                fighter.hp, objects[PLAYER].get_max_hp(game),
                player.get_power(game),
                player.get_defense(game),
                fighter.level,
                fighter.xp, (LEVEL_XP_RANGE[fighter.level as usize] - fighter.xp));
                msgbox(&mut tcod.root, &msg, STATS_SCREEN_WIDTH);
            }
            PlayerAction::DidntTakeTurn
        }
        _ => PlayerAction::DidntTakeTurn,
    }
}

/// Populates a Tcod's FOV according to a Game's map, determining player sight
fn init_fov(tcod: &mut Tcod, map: &Map) {
    for y in 0..MAP_HEIGHT {
        for x in 0..MAP_WIDTH {
            tcod.fov.set(x, y,
                         !map[x as usize][y as usize].block_sight,
                         !map[x as usize][y as usize].blocked);
        }
    }
    tcod.con.clear(); // Make unexplored areas are black (default bg color)
    // Note: This is necessary for erasing previous games' or levels' FOV maps from the screen.
}

/// Sets up the Game struct, FOV map, etc.
fn new_game(tcod: &mut Tcod) -> (Vec<Object>, Game) {
    let mut player = Object::new(0, 0, '@', colors::WHITE, "player", true);
    player.alive = true;
    player.fighter = Some(Fighter{ base_max_hp: 100,
                                   hp: 100,
                                   base_defense: 1,
                                   base_power: 2,
                                   xp: 0,
                                   level: 0,
                                   on_death: DeathCallback::Player });

    let mut objects = vec![player];
    let first_level = 1;

    let mut game = Game {
        map: make_map(&mut objects, first_level),
        log: vec![],
        inv: vec![],
        dungeon_level: 1,
    };

    init_fov(tcod, &game.map);

    // Give a warm, welcoming message
    game.log.add("You enter the ergosphere...", colors::RED);
    // and a helpful piece of gear!
    let mut dagger = Object::new(0, 0, '-', colors::SKY, "dagger", false);
    dagger.item = Some(Item::Sword); // TODO: Make this its own Item::Dagger
    dagger.equipment = Some(Equipment {
        equipped: true,
        slot: Slot::RightHand,
        hp_buff: 0,
        power_buff: 1,
        defense_buff: 0,
    });
    game.inv.push(dagger);

    (objects, game)
}

/// Saves game
/// Returns Box<Error> to be able to return any type implementing Error trait since
/// serde::to_string(), File::create(), and File::write_all() return different error types.
/// This is just preference; the ? operator already converts any errors to whatever other error
/// type we'd return.
fn save_game(objects: &[Object], game: &Game) -> Result<(), Box<Error>> {
    let save_data = serde_json::to_string(&(objects, game))?;
    let mut file = File::create("savegame")?;

    file.write_all(save_data.as_bytes())?;
    Ok(())
}

/// Loads a save
fn load_game() -> Result<(Vec<Object>, Game), Box<Error>> {
    let mut json_save_state = String::new();
    let mut file = File::open("savegame")?;

    file.read_to_string(&mut json_save_state)?;
    let result = serde_json::from_str::<(Vec<Object>, Game)>(&json_save_state)?;
    Ok(result)
}

/// Runs game loop
fn play_game(tcod: &mut Tcod, objects: &mut Vec<Object>, game: &mut Game) {
    let mut prev_player_pos = (-1, -1); // Force first FOV compute in game loop

    let mut key = Default::default();

    while !tcod.root.window_closed() {
        match input::check_for_event(input::MOUSE | input::KEY_PRESS) {
            // Check mouse and keyboard states in real time, not just once per turn!
            // This allows for realtime rendering of mouse look info, and
            // check_for_event()'s key state replaced wait_for_keypress() in handle_keys().
            Some((_, Event::Mouse(m))) => tcod.mouse = m,
            Some((_, Event::Key(k))) => key = k,
            // Clear key back to default state when getting no event because handle_keys() would
            // treat it as a keypress.
            _ => key = Default::default(),
            // Since we only use mouse for looking, we don't have to clear it too.
        }

        // Render screen update if player moved
        let fov_recompute = prev_player_pos != (objects[PLAYER].pos());
        render_all(tcod, &objects, game, fov_recompute);
        tcod.root.flush(); // Presents root's content update to the screen

        level_up(tcod, objects, game); // Check for player ding; TODO: move this to a reward_XP()

        // Erase all objects at their old locations before they move
        for object in objects.iter_mut() {
            object.clear(&mut tcod.con)
        }

        // Handle user actions
        prev_player_pos = objects[PLAYER].pos();
        let player_action = handle_keys(key, tcod, objects, game);
        if player_action == PlayerAction::Exit {
            save_game(objects, game).unwrap(); // Panic if Err returned
            break
        }

        // Let NPCs take turn
        if objects[PLAYER].alive && player_action != PlayerAction::DidntTakeTurn {
            for id in 0..objects.len() {
                if objects[id].ai.is_some() {
                    ai_take_turn(id, &tcod.fov, objects, game);
                }
            }
        }
    }
}

/// Shows main menu before and after games
fn main_menu(tcod: &mut Tcod) {
    // 160x100 pixel background image displayed on a 80x50 console via subcell resolution:
    // http://roguecentral.org/doryen/data/libtcod/doc/1.5.1/html2/image_blit.html?c=true
    let img = tcod::image::Image::from_file("main_menu_background.png")
        .ok().expect("ERROR: main_menu_background.png not found!"); // Exit if failed

    // Showing menu in a loop lets us play another game after one ends
    while !tcod.root.window_closed() {
        // Show background image at twice console resolution
        tcod::image::blit_2x(&img, (0, 0), (-1, -1), &mut tcod.root, (0, 0));

        tcod.root.set_default_foreground(colors::WHITE);
        tcod.root.print_ex(SCREEN_WIDTH / 2, SCREEN_HEIGHT / 2 - 4,
                           BackgroundFlag::None, TextAlignment::Center,
                           "Dissident in the Ergosphere");
        tcod.root.print_ex(SCREEN_WIDTH / 2, SCREEN_HEIGHT - 2,
                           BackgroundFlag::None, TextAlignment::Center,
                           format!("v{}", VERSION));

        // Menu options
        let choices = &["Start", "Continue", "Quit"];
        let choice = menu("", choices, 24, &mut tcod.root);

        match choice {
            Some(0) => { // Start
                let (mut objects, mut game) = new_game(tcod);
                play_game(tcod, &mut objects, &mut game);
            }
            Some(1) => { // Continue
                match load_game() {
                    Ok((mut objects, mut game)) => {
                        init_fov(tcod, &game.map);
                        play_game(tcod, &mut objects, &mut game);
                    }
                    Err(_e) => {
                        msgbox(&mut tcod.root, "\nSave not found.\n", 24);
                        continue;
                    }
                }
            }
            Some(2) => { // Quit
                break;
            }
            _ => {} // Don't respond to unrecognized commands
        }
    }
}

fn main() {
    // System initialization (tcod window and consoles)
    let root = Root::initializer()
        .font("terminal10x10_gs_tc.png", FontLayout::Tcod)
        .font_type(FontType::Greyscale)
        .size(SCREEN_WIDTH, SCREEN_HEIGHT)
        .title("Rust/libtcod tutorial")
        .init();
    tcod::system::set_fps(LIMIT_FPS);

    let mut tcod = Tcod {
        root: root,
        con: Offscreen::new(MAP_WIDTH, MAP_HEIGHT),
        panel: Offscreen::new(SCREEN_WIDTH, PANEL_HEIGHT),
        fov: FovMap::new(MAP_WIDTH, MAP_HEIGHT),
        mouse: Default::default(),
    };

    main_menu(&mut tcod);
}
