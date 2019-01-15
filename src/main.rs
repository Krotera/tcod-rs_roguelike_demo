extern crate tcod;
extern crate rand;

use std::cmp;

use tcod::console::*;
use tcod::colors::{self, Color};
use tcod::input::{self, Event, Key, Mouse};
use tcod::map::{Map as FovMap, FovAlgorithm}; // We use "Map" for our tile array; rename to FovMap
use rand::Rng;

// Tutorial: https://tomassedovic.github.io/roguelike-tutorial/index.html
// tcod-rs library docs: https://tomassedovic.github.io/tcod-rs/tcod/index.html
/******************************************************************************
Consts and types
*******************************************************************************/

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
const MAX_ROOM_MONSTERS: i32 = 3;
const MAX_ROOM_ITEMS: i32 = 2;

const COLOR_DARK_WALL: Color = Color { r: 0, g: 0, b: 100 };
const COLOR_LIGHT_WALL: Color = Color { r: 130, g: 110, b: 50 };
const COLOR_DARK_GROUND: Color = Color { r: 50, g: 50, b: 150 };
const COLOR_LIGHT_GROUND: Color = Color { r: 200, g: 180, b: 50 };

// http://www.roguebasin.com/index.php?title=Comparative_study_of_field_of_view_algorithms_for_2D_grid_based_worlds
const FOV_ALGO: FovAlgorithm = FovAlgorithm::Basic;
const FOV_LIGHT_WALLS: bool = true;
const TORCH_RADIUS: i32 = 10;

const HEAL_AMOUNT: i32 = 4;

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

type Map = Vec<Vec<Tile>>; // 2D map of tiles
type Messages = Vec<(String, Color)>;

/******************************************************************************
Structs and enums
*******************************************************************************/

/// A map tile
#[derive(Clone, Copy, Debug)]
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
#[derive(Debug)]
struct Object {
    x: i32,
    y: i32,
    char: char,
    color: Color,
    name: String,
    blocks: bool,
    alive: bool,
    // Components
    fighter: Option<Fighter>,
    ai: Option<Ai>,
    item: Option<Item>,
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
            alive: false,
            // Components
            fighter: None,
            ai: None,
            item: None,
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
        ((dx.pow(2) + dy.pow(2)) as f32).sqrt()
    }

    pub fn take_damage(&mut self, damage: i32, messages: &mut Messages) {
        // Apply damage if possible
        if let Some(fighter) = self.fighter.as_mut() { // Mutable borrow!
            if damage > 0 {
                fighter.hp -= damage;
            }
            // Check for death and call death function
            if fighter.hp <= 0 {
                self.alive = false;
                fighter.on_death.callback(self, messages);
            }
        }
        /*
        // TODO: Remove
        // Check for death and call death function
        if let Some(fighter) = self.fighter { // This is a COPY of fighter, not a borrow!
            if fighter.hp <= 0 {
                self.alive = false;
                fighter.on_death.callback(self);
                // So changes to it here wouldn't be reflected in self.fighter.
            }
        }*/
    }

    pub fn attack(&mut self, target: &mut Object, messages: &mut Messages) {
        // Simple formula for attack damage
        let damage = self.fighter.map_or(0, |f| f.power) - target.fighter.map_or(0, |f| f.defense);
        if damage > 0 {
            message(messages,
                    format!("{} attacks {} for {} HP.", self.name, target.name, damage),
                    colors::WHITE);
            target.take_damage(damage, messages);
        } else {
            message(messages,
                    format!("{} attacks {} but it has no effect.", self.name, target.name),
                    colors::WHITE);
        }
    }

    // Heal up to max_hp by given amount
    fn heal(&mut self, amount: i32) {
        if let Some(ref mut fighter) = self.fighter {
            fighter.hp += amount;
            if fighter.hp > fighter.max_hp {
                fighter.hp = fighter.max_hp;
            }
        }
    }
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

// Able to attack or be attacked
#[derive(Clone, Copy, Debug, PartialEq)]
struct Fighter {
    max_hp: i32,
    hp: i32,
    defense: i32,
    power: i32,
    on_death: DeathCallback, // Death behavior
}

// Things with a brain... or not
#[derive(Clone, Copy, Debug, PartialEq)]
struct Ai;

#[derive(Clone, Copy, Debug, PartialEq)]
enum PlayerAction {
    TookTurn,
    DidntTakeTurn,
    Exit,
}

// Item "categories" associated with an effect function in use_item()
#[derive(Clone, Copy, Debug, PartialEq)]
enum Item {
    Heal,
}

// On death, monsters drop loot and leave corpses, the player loses the game, etc.
// These enums are used by component structs to know what function to call on an object's death.
#[derive(Clone, Copy, Debug, PartialEq)]
enum DeathCallback {
    Player,
    Monster,
}

impl DeathCallback {
    fn callback(self, object: &mut Object, messages: &mut Messages) {
        let callback: fn(&mut Object, &mut Messages) = match self {
            DeathCallback::Player => player_death,
            DeathCallback::Monster => monster_death,
        };
        callback(object, messages);
    }
}

// Item after-use states, determining item consumption
enum UseResult {
    UsedUp,
    Cancelled,
}

/******************************************************************************
UI functions
*******************************************************************************/

/// Renders a colored stat bar (representing HP, XP, etc.)
fn render_bar(panel: &mut Offscreen, x: i32, y: i32, total_width: i32, name: &str,
              value: i32, max: i32, bar_color: Color, back_color: Color) {
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

/// Adds a message to the log, taking anything T that implements Into trait for String (i.e.,
/// can be converted to String)
fn message<T: Into<String>>(messages: &mut Messages, message: T, color: Color) {
    // If buffer full, remove first message to make room
    if messages.len() == MSG_HEIGHT {
        messages.remove(0);
    }
    messages.push((message.into(), color));
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

/// Menu screen where the user can select items by their corresponding letter keys
/// header - Window title or description (e.g., "Inventory", "Choose an item.")
/// options - List of items
/// width - of the window (height is implicit as it depends on header height and number of options)
/// root - Main console
fn menu<T: AsRef<str>>(header: &str, options: &[T], width: i32,
                       root: &mut Root) -> Option<usize> {
    assert!(options.len() <= 26, "Cannot have a menu with more than 26 options (for now)!");
    // Calc total height of header (after auto-wrap) and one line per option
    let header_height = root.get_height_rect(0, 0, width, SCREEN_HEIGHT, header);
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
        inventory.iter().map(|item| { item.name.clone() }).collect()
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

fn is_blocked(x: i32, y: i32, map: &Map, objects: &[Object]) -> bool {
    // First test map tile
    if map[x as usize][y as usize].blocked {
        return true;
    }
    // Then check for blocking objects
    objects.iter().any(|object| {
        object.blocks && object.pos() == (x, y)
    })
}

/// Move an object by the given amount if the destination isn't blocked.
fn move_by(id: usize, dx: i32, dy: i32, map: &Map, objects: &mut [Object]) {
    let (x, y) = objects[id].pos();
    if !is_blocked(x + dx, y + dy, map, objects) {
        objects[id].set_pos(x + dx, y + dy);
    }
}

/// Moves the player to an unblocked tile or attacks a hostile actor on that tile
fn player_move_or_attack(dx: i32, dy: i32, map: &Map, objects: &mut [Object],
                         messages: &mut Messages) {
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
            player.attack(target, messages);
        }
        None => {
            move_by(PLAYER, dx, dy, map, objects);
        }
    }
}

// Move one square towards a target
fn move_towards(id: usize, target_x: i32, target_y: i32, map: &Map, objects: &mut [Object]) {
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
    move_by(id, dx, dy, map, objects);
}

/// Mutably borrow two *separate* elements from the given slice.
/// Panics when the indexes are equal or out of bounds.
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

// Basic behavior: if you see a monster, it can see you. If it can see you, it will approach until
// in melee range and attack.
fn ai_take_turn(monster_id: usize, map: &Map, objects: &mut [Object], fov_map: &FovMap,
                messages: &mut Messages) {
    let (monster_x, monster_y) = objects[monster_id].pos();
    if fov_map.is_in_fov(monster_x, monster_y) {
        if objects[monster_id].distance_to(&objects[PLAYER]) >= 2.0 { // Move towards player
            let (player_x, player_y) = objects[PLAYER].pos();
            move_towards(monster_id, player_x, player_y, map, objects);
        } else if objects[PLAYER].fighter.map_or(false, |f| f.hp > 0) { // Attack (if player still alive)
            //let monster = &mut objects[monster_id];
            //monster.attack(&mut objects[PLAYER]); // ERROR: Double mutable borrow!
            // Rust doesn't know if objects[PLAYER] is distinct from objects[momster_id], meaning
            // there's potentially two mutable borrows to the same thing, which is forbidden.
            // Use mut_two() as workaround.
            let (monster, player) = mut_two(monster_id, PLAYER, objects);
            monster.attack(player, messages);
        }
    }
}

fn player_death(player: &mut Object, messages: &mut Messages) {
    // Game over!
    message(messages, "You died!", colors::RED);

    // Turn player into corpse
    player.char = '%';
    player.color = colors::DARK_RED;
}

fn monster_death(monster: &mut Object, messages: &mut Messages) {
    // Turn monster into corpse
    message(messages, format!("{} dies.", monster.name), colors::ORANGE);
    monster.char = '%';
    monster.color = colors::DARK_RED;
    monster.blocks = false;
    monster.fighter = None;
    monster.ai = None;
    monster.name = format!("{} corpse", monster.name);
}

fn pick_item_up(object_id: usize, objects: &mut Vec<Object>, inventory: &mut Vec<Object>,
                messages: &mut Messages) {
    if inventory.len() >= 26 { // Arbitrary limit to map items to letter keys for selection
        message(messages,
                format!("Your inventory is full."),
                colors::WHITE);
    } else {
        let item = objects.swap_remove(object_id);
        message(messages,
                format!("Picked up {}.", item.name),
                colors::GREEN);
        inventory.push(item); // TODO: Then... actually ADD item to the inventory.
    }
}

/// Uses an inventory item, executing its effect function
fn use_item(inventory_id: usize, inventory: &mut Vec<Object>,
            objects: &mut [Object], messages: &mut Messages) {
    // Attempt to get item
    if let Some(item) = inventory[inventory_id].item {
        let on_use = match item { // Get item's effect function
            Item::Heal => cast_heal,
        };

        // If we have an item, match its effect type
        match on_use(inventory_id, objects, messages) {
            UseResult::UsedUp => { // Destroy after use (unless it was cancelled)
                inventory.remove(inventory_id);
            }
            UseResult::Cancelled => {
                message(messages, "Cancelled", colors::WHITE);
            }
        }
    } else {
        message(
            messages,
            format!("{} cannot be used.", inventory[inventory_id].name),
            colors::WHITE
        );
    }
}

/// Player heal item effect function, associated with Item::Heal in use_item()
/// It just carries out the effect logic, including calling the player object's heal function,
/// and returns an appropriate UseResult for the responsible item.
fn cast_heal(inventory_id: usize, objects: &mut [Object], messages: &mut Messages) -> UseResult {
    if let Some(fighter) = objects[PLAYER].fighter {
        if fighter.hp == fighter.max_hp {
            message(messages, "You're already at full HP!", colors::RED);
            return UseResult::Cancelled;
        }
        message(messages, "Your wounds begin knitting themselves shut.", colors::LIGHT_VIOLET);
        // Call player object heal function with HEAL_AMOUNT associated with the item type of
        // objects[inventory_id]
        objects[PLAYER].heal(HEAL_AMOUNT);
        return UseResult::UsedUp;
    }
    UseResult::Cancelled
}

/******************************************************************************
Tile-based functions
*******************************************************************************/

// Clears an area of the map to create a room
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

fn place_objects(room: Rect, map: &Map, objects: &mut Vec<Object>) {
    // Spawn random number of monsters per room
    let num_monsters = rand::thread_rng().gen_range(0, MAX_ROOM_MONSTERS + 1);

    for _ in 0..num_monsters {
        let x = rand::thread_rng().gen_range(room.x1 + 1, room.x2);
        let y = rand::thread_rng().gen_range(room.y1 + 1, room.y2);

        if !is_blocked(x, y, map, objects) {
            let mut monster = if rand::random::<f32>() < 0.8 { // 80% chance for an orc
                let mut orc = Object::new(x, y, 'o', colors::DESATURATED_GREEN, "orc", true);
                orc.fighter = Some(Fighter{
                    max_hp: 10,
                    hp: 10,
                    defense: 0,
                    power: 3,
                    on_death: DeathCallback::Monster,
                });
                orc.ai = Some(Ai);
                orc
            } else {
                let mut troll = Object::new(x, y, 'T', colors::DARKER_GREEN, "troll", true);
                troll.fighter = Some(Fighter{
                    max_hp: 16,
                    hp: 16,
                    defense: 1,
                    power: 4,
                    on_death: DeathCallback::Monster,
                });
                troll.ai = Some(Ai);
                troll
            };
            monster.alive = true; // It's alive!!!
            objects.push(monster);
        }
    }

    // Spawn random number of items per room
    let num_items = rand::thread_rng().gen_range(0, MAX_ROOM_ITEMS + 1);

    for _ in 0..num_items {
        let x = rand::thread_rng().gen_range(room.x1 + 1, room.x2);
        let y = rand::thread_rng().gen_range(room.y1 + 1, room.y2);

        if !is_blocked(x, y, map, objects) {
            // Randomly-dropped healing potion
            let mut object = Object::new(x, y, '!', colors::VIOLET, "healing potion", false);
            object.item = Some(Item::Heal);
            objects.push(object);
        }
    }
}

fn make_map(objects: &mut Vec<Object>) -> Map {
    // Fill map with wall tiles (underground)
    let mut map = vec![vec![Tile::new_wall(); MAP_HEIGHT as usize]; MAP_WIDTH as usize];

    // Hollow out {MAX_ROOMS} randomly-sized rooms
    let mut rooms: Vec<Rect> = vec![]; // Reference of rooms for tunnel rendering, etc.
    //let mut starting_position = (0, 0); // Player start (to-be center of first room)

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
            place_objects(new_room, &map, objects);

            rooms.push(new_room);
        }
    }

    map
}

fn render_all(root: &mut Root, con: &mut Offscreen, panel: &mut Offscreen, mouse: Mouse,
              map: &mut Map, objects: &[Object], messages: &Messages,
              fov_map: &mut FovMap, fov_recompute: bool) {
    if fov_recompute { // Player moved
        let player = &objects[PLAYER];
        fov_map.compute_fov(player.x, player.y, TORCH_RADIUS, FOV_LIGHT_WALLS, FOV_ALGO);

        // Draw all map tiles, shading by visibility using FovMap
        for y in 0..MAP_HEIGHT {
            for x in 0..MAP_WIDTH {
                let visible = fov_map.is_in_fov(x, y);
                let wall = map[x as usize][y as usize].block_sight;
                let color = match (visible, wall) {
                    // Outside FOV
                    (false, true) => COLOR_DARK_WALL,
                    (false, false) => COLOR_DARK_GROUND,
                    // Inside FOV
                    (true, true) => COLOR_LIGHT_WALL,
                    (true, false) => COLOR_LIGHT_GROUND,
                };

                let explored = &mut map[x as usize][y as usize].explored;
                if visible { // If tile seen, it's been explored.
                    *explored = true;
                }
                if *explored { // If tile explored, render it.
                    con.set_char_background(x, y, color, BackgroundFlag::Set);
                }
            }
        }
    }

    // Set object render hierarchy (e.g., live actors cover corpses and items), but only bother
    // if they're visible (in FOV)
    // Note: render_all is borrowing objects as &[Object], so it can't change it (nor should it)
    let mut to_draw: Vec<_> = objects.iter().filter(|o| fov_map.is_in_fov(o.x, o.y)).collect();
    // Sort so that non-blocking objects come first
    // Note: |a, b| tacitly means "a is less than b", so the a.cmp(b) = Ordering::Less, which sorts
    // the container in ascending order;
    // b.cmp(a) = Ordering::Greater would sort it in descending order;
    // and false is considered less than true.
    to_draw.sort_unstable_by(|o1, o2| { o1.blocks.cmp(&o2.blocks) }); // false < true ascending
    // Draw all objects
    for object in &to_draw {
        object.draw(con);
    }

    // Blit con onto root console
    // Docs: https://tomassedovic.github.io/tcod-rs/tcod/console/fn.blit.html
    blit(con, (0, 0), (MAP_WIDTH, MAP_HEIGHT), root, (0, 0), 1.0, 1.0);

    // Begin rendering GUI panel
    panel.set_default_background(colors::BLACK);
    panel.clear();

    // Print all messages, using libtcod's get_height_rect() and print_rect() for line wrapping
    let mut y = MSG_HEIGHT as i32;
    for &(ref msg, color) in messages.iter().rev() { // .rev() to start with last message
        // Calc height of message to wrap
        let msg_height = panel.get_height_rect(MSG_X, y, MSG_WIDTH, 0, msg);

        y -= msg_height; // Print message upwards
        if y < 0 { // Out of room; we're printing above the panel (which is invisible anyway),
            break; // so stop.
        }
        panel.set_default_foreground(color);
        panel.print_rect(MSG_X, y, MSG_WIDTH, 0, msg);
    }

    // Show player stats & bars
    let hp = objects[PLAYER].fighter.map_or(0, |f| f.hp);
    let max_hp = objects[PLAYER].fighter.map_or(0, |f| f.max_hp);
    render_bar(panel, 1, 1, BAR_WIDTH, "HP", hp, max_hp, colors::LIGHT_RED, colors::DARKER_RED);

    // display names of objects under the mouse
    panel.set_default_foreground(colors::LIGHT_GREY);
    panel.print_ex(1, 0, BackgroundFlag::None, TextAlignment::Left,
                   get_names_under_mouse(mouse, objects, fov_map));

    // Blit panel contents to root console
    // Docs: https://tomassedovic.github.io/tcod-rs/tcod/console/fn.blit.html
    blit(panel, (0, 0), (SCREEN_WIDTH, PANEL_HEIGHT), root, (0, PANEL_Y), 1.0, 1.0);

    // Con stuff... remove, probably. : T
    if let Some(fighter) = objects[PLAYER].fighter {
        root.print_ex(1, SCREEN_HEIGHT - 2, BackgroundFlag::None, TextAlignment::Left,
                        format!("HP: {}/{} ", fighter.hp, fighter.max_hp));
    }
}

/// Carries out keymapped actions and returns their PlayerAction type.
/// In-game which result in taking a turn (e.g., moving) return TookTurn; meta-game actions
/// (e.g., toggling fullscreen) return other PlayerActions.
fn handle_keys(key: Key, root: &mut Root, map: &Map,
               objects: &mut Vec<Object>, inventory: &mut Vec<Object>,
               messages: &mut Messages) -> PlayerAction {
    use tcod::input::Key;
    use tcod::input::KeyCode::*;

    let player_alive = objects[PLAYER].alive;
    match (key, player_alive) { // Only allow some actions if not game over
        // Meta actions
        (Key { code: Enter, alt: true, .. }, _) => { // Alt+Enter: toggle fullscreen
            let fullscreen = root.is_fullscreen();
            root.set_fullscreen(!fullscreen);
            PlayerAction::DidntTakeTurn
        }
        (Key { code: Escape, .. }, _) => PlayerAction::Exit, // Esc: exit

        // Movement
        // Note: ~1 sec movement delay when changing spammed direction seems like
        // industry standard (same effect in Crawl, etc.).
        // Arrow
        (Key { code: Up, .. }, true) => {
            player_move_or_attack(0, -1, map, objects, messages);
            PlayerAction::TookTurn
        }
        (Key { code: Down, .. }, true) => {
            player_move_or_attack(0, 1, map, objects, messages);
            PlayerAction::TookTurn
        }
        (Key { code: Left, .. }, true) => {
            player_move_or_attack(-1, 0, map, objects, messages);
            PlayerAction::TookTurn
        }
        (Key { code: Right, .. }, true) => {
            player_move_or_attack(1, 0, map, objects, messages);
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
            player_move_or_attack(-1, 1, map, objects, messages);
            PlayerAction::TookTurn
        }
        (Key { code: NumPad2, .. }, true) => {
            player_move_or_attack(0, 1, map, objects, messages);
            PlayerAction::TookTurn
        }
        (Key { code: NumPad3, .. }, true) => {
            player_move_or_attack(1, 1, map, objects, messages);
            PlayerAction::TookTurn
        }
        (Key { code: NumPad4, .. }, true) => {
            player_move_or_attack(-1, 0, map, objects, messages);
            PlayerAction::TookTurn
        }
        /*(Key { code: NumPad5, .. }, true) => {

        }*/
        (Key { code: NumPad6, .. }, true) => {
            player_move_or_attack(1, 0, map, objects, messages);
            PlayerAction::TookTurn
        }
        (Key { code: NumPad7, .. }, true) => {
            player_move_or_attack(-1, -1, map, objects, messages);
            PlayerAction::TookTurn
        }
        (Key { code: NumPad8, .. }, true) => {
            player_move_or_attack(0, -1, map, objects, messages);
            PlayerAction::TookTurn
        }
        (Key { code: NumPad9, .. }, true) => {
            player_move_or_attack(1, -1, map, objects, messages);
            PlayerAction::TookTurn
        }
        /*(Key { code: NumPad0, .. }, true) => {

        }*/
        /* WASDQEZC

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
        }
        /* Vi

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
        }
        // Actions
        (Key { printable: 'g', .. }, true) => { // Pick up item
            let item_id = objects.iter().position(|object| {
                object.pos() == objects[PLAYER].pos() && object.item.is_some()
            });
            if let Some(item_id) = item_id {
                pick_item_up(item_id, objects, inventory, messages);
            }
            PlayerAction::DidntTakeTurn
        }
        (Key { printable: 'i', .. }, true) => { // Show inventory
            let inventory_index = inventory_menu(
                inventory,
                "Select an item by pressing its key or any other key to cancel.\n",
                root
            );
            if let Some(inventory_index) = inventory_index {
                use_item(inventory_index, inventory, objects, messages);
            }
            PlayerAction::DidntTakeTurn
        }

        _ => PlayerAction::DidntTakeTurn,
    }
}

fn main() {
    // Main console
    let mut root = Root::initializer()
        .font("terminal10x10_gs_tc.png", FontLayout::Tcod)
        .font_type(FontType::Greyscale)
        .size(SCREEN_WIDTH, SCREEN_HEIGHT)
        .title("Rust/libtcod tutorial")
        .init();
    tcod::system::set_fps(LIMIT_FPS);

    // Side panels
    let mut con = Offscreen::new(MAP_WIDTH, MAP_HEIGHT);
    let mut panel = Offscreen::new(SCREEN_WIDTH, PANEL_HEIGHT);

    // Message log
    let mut messages = vec![];
    // A warm, welcoming message!
    message(&mut messages, "Ahh, fresh meat!", colors::RED);

    // Create player
    let mut player = Object::new(0, 0, '@', colors::WHITE, "player", true); // Put in center of a room
    player.alive = true;
    player.fighter = Some(Fighter{
        max_hp: 30,
        hp: 30,
        defense: 2,
        power: 5,
        on_death: DeathCallback::Player,
    });

    // Store all objects in a container, with the player as the 0th object
    let mut objects = vec![player];

    // Generate map and player starting position
    let mut map = make_map(&mut objects);

    // FovMap's database of sight-blocking tiles for determining visibility
    let mut fov_map = FovMap::new(MAP_WIDTH, MAP_HEIGHT);
    for y in 0..MAP_HEIGHT {
        for x in 0..MAP_WIDTH {
            // set(&mut self, x: i32, y: i32, transparent: bool, walkable: bool)
            // Our tiles specify blockage, not passage, so invert bools.
            fov_map.set(x, y,
                        !map[x as usize][y as usize].block_sight,
                        !map[x as usize][y as usize].blocked);
        }
    }
    // Store prev pos to determine necessity of recomputing FOV, force first time
    let mut prev_player_position = (-1, -1);

    // Player's inv
    let mut inventory: Vec<Object> = vec![];

    let mut mouse = Default::default(); // Mouse look functionality
    let mut key = Default::default();

    // THE MARVELOUS GAME LOOP
    while !root.window_closed() {
        // Check mouse and keyboard states in real time, not just once per turn!
        // This allows for realtime rendering of mouse look info, and
        // check_for_event()'s key state replaced wait_for_keypress() in handle_keys().
        match input::check_for_event(input::MOUSE | input::KEY_PRESS) {
            Some((_, Event::Mouse(m))) => mouse = m,
            Some((_, Event::Key(k))) => key = k,
            // Clear key back to default state when getting no event because handle_keys() would
            // treat it as a keypress.
            _ => key = Default::default(),
            // Since we only use mouse for looking, we don't have to clear it too.
        }

        // Render update
        let fov_recompute = prev_player_position != objects[PLAYER].pos();
        render_all(&mut root, &mut con, &mut panel, mouse, &mut map, &objects, &messages,
            &mut fov_map, fov_recompute);

        root.flush(); // Presents root's content update to the screen

        //root.clear(); // Wipes old frames from screen (Old way)
        // Erase all objects at their old locations before they move
        for object in &objects {
            object.clear(&mut con)
        }

        //let player = &mut objects[PLAYER];
        prev_player_position = objects[PLAYER].pos(); // Record player pos before move
        // Handle key input and exit if needed
        let player_action = handle_keys(key, &mut root, &map, &mut objects, &mut inventory,
            &mut messages);
        if player_action == PlayerAction::Exit {
            break
        }

        // Let NPCs take their turn
        if objects[PLAYER].alive && player_action != PlayerAction::DidntTakeTurn {
            for id in 0..objects.len() {
                if objects[id].ai.is_some() {
                    ai_take_turn(id, &map, &mut objects, &fov_map, &mut messages);
                }
            }
        }
    }
}
