package main

import (
	"bufio"
	"encoding/json"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strconv"
	"strings"
	"unicode"
)

// LSP Protocol structures
type Message struct {
	Jsonrpc string      `json:"jsonrpc"`
	ID      interface{} `json:"id,omitempty"`
	Method  string      `json:"method,omitempty"`
	Params  interface{} `json:"params,omitempty"`
	Result  interface{} `json:"result,omitempty"`
	Error   *RPCError   `json:"error,omitempty"`
}

type RPCError struct {
	Code    int         `json:"code"`
	Message string      `json:"message"`
	Data    interface{} `json:"data,omitempty"`
}

type InitializeParams struct {
	ProcessID             int                `json:"processId"`
	RootPath              string             `json:"rootPath,omitempty"`
	RootURI               string             `json:"rootUri,omitempty"`
	InitializationOptions interface{}        `json:"initializationOptions,omitempty"`
	Capabilities          ClientCapabilities `json:"capabilities"`
	Trace                 string             `json:"trace,omitempty"`
	WorkspaceFolders      []WorkspaceFolder  `json:"workspaceFolders,omitempty"`
}

type ClientCapabilities struct {
	TextDocument *TextDocumentClientCapabilities `json:"textDocument,omitempty"`
	Workspace    *WorkspaceClientCapabilities    `json:"workspace,omitempty"`
}

type TextDocumentClientCapabilities struct {
	Completion *CompletionClientCapabilities `json:"completion,omitempty"`
}

type CompletionClientCapabilities struct {
	CompletionItem *CompletionItemClientCapabilities `json:"completionItem,omitempty"`
}

type CompletionItemClientCapabilities struct {
	SnippetSupport bool `json:"snippetSupport,omitempty"`
}

type WorkspaceClientCapabilities struct {
	WorkspaceFolders bool `json:"workspaceFolders,omitempty"`
}

type WorkspaceFolder struct {
	URI  string `json:"uri"`
	Name string `json:"name"`
}

type ServerCapabilities struct {
	TextDocumentSync       int                    `json:"textDocumentSync"`
	CompletionProvider     *CompletionOptions     `json:"completionProvider,omitempty"`
	HoverProvider          bool                   `json:"hoverProvider"`
	DocumentSymbolProvider bool                   `json:"documentSymbolProvider"`
	SemanticTokensProvider *SemanticTokensOptions `json:"semanticTokensProvider,omitempty"`
}

type SemanticTokensOptions struct {
	Legend SemanticTokensLegend `json:"legend"`
	Full   bool                 `json:"full"`
	Range  bool                 `json:"range"`
}

type SemanticTokensLegend struct {
	TokenTypes     []string `json:"tokenTypes"`
	TokenModifiers []string `json:"tokenModifiers"`
}

type SemanticTokensParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
}

type SemanticTokens struct {
	ResultID string `json:"resultId,omitempty"`
	Data     []int  `json:"data"`
}

type CompletionOptions struct {
	ResolveProvider   bool     `json:"resolveProvider"`
	TriggerCharacters []string `json:"triggerCharacters"`
}

type Position struct {
	Line      int `json:"line"`
	Character int `json:"character"`
}

type Range struct {
	Start Position `json:"start"`
	End   Position `json:"end"`
}

type Location struct {
	URI   string `json:"uri"`
	Range Range  `json:"range"`
}

type TextDocumentIdentifier struct {
	URI string `json:"uri"`
}

type TextDocumentPositionParams struct {
	TextDocument TextDocumentIdentifier `json:"textDocument"`
	Position     Position               `json:"position"`
}

type CompletionParams struct {
	TextDocumentPositionParams
	Context *CompletionContext `json:"context,omitempty"`
}

type CompletionContext struct {
	TriggerKind      int    `json:"triggerKind"`
	TriggerCharacter string `json:"triggerCharacter,omitempty"`
}

type CompletionItem struct {
	Label         string      `json:"label"`
	Kind          int         `json:"kind,omitempty"`
	Detail        string      `json:"detail,omitempty"`
	Documentation string      `json:"documentation,omitempty"`
	InsertText    string      `json:"insertText,omitempty"`
	Data          interface{} `json:"data,omitempty"`
}

type CompletionList struct {
	IsIncomplete bool             `json:"isIncomplete"`
	Items        []CompletionItem `json:"items"`
}

type Hover struct {
	Contents interface{} `json:"contents"`
	Range    *Range      `json:"range,omitempty"`
}

type DidOpenTextDocumentParams struct {
	TextDocument TextDocumentItem `json:"textDocument"`
}

type TextDocumentItem struct {
	URI        string `json:"uri"`
	LanguageID string `json:"languageId"`
	Version    int    `json:"version"`
	Text       string `json:"text"`
}

type DidChangeTextDocumentParams struct {
	TextDocument   VersionedTextDocumentIdentifier  `json:"textDocument"`
	ContentChanges []TextDocumentContentChangeEvent `json:"contentChanges"`
}

type VersionedTextDocumentIdentifier struct {
	TextDocumentIdentifier
	Version int `json:"version"`
}

type TextDocumentContentChangeEvent struct {
	Range       *Range `json:"range,omitempty"`
	RangeLength int    `json:"rangeLength,omitempty"`
	Text        string `json:"text"`
}

// Semantic token types (standard LSP types)
const (
	SemanticTokenTypeNamespace = iota
	SemanticTokenTypeType
	SemanticTokenTypeClass
	SemanticTokenTypeEnum
	SemanticTokenTypeInterface
	SemanticTokenTypeStruct
	SemanticTokenTypeTypeParameter
	SemanticTokenTypeParameter
	SemanticTokenTypeVariable
	SemanticTokenTypeProperty
	SemanticTokenTypeEnumMember
	SemanticTokenTypeEvent
	SemanticTokenTypeFunction
	SemanticTokenTypeMethod
	SemanticTokenTypeMacro
	SemanticTokenTypeKeyword
	SemanticTokenTypeModifier
	SemanticTokenTypeComment
	SemanticTokenTypeString
	SemanticTokenTypeNumber
	SemanticTokenTypeRegexp
	SemanticTokenTypeOperator
)

// Token modifiers
const (
	SemanticTokenModifierDeclaration = iota
	SemanticTokenModifierDefinition
	SemanticTokenModifierReadonly
	SemanticTokenModifierStatic
	SemanticTokenModifierDeprecated
	SemanticTokenModifierAbstract
	SemanticTokenModifierAsync
	SemanticTokenModifierModification
	SemanticTokenModifierDocumentation
	SemanticTokenModifierDefaultLibrary
)

// GML-specific token information
type GMLToken struct {
	Start     int
	Length    int
	TokenType int
	Modifiers int
	Line      int
	Character int
}

const (
	CompletionItemKindText          = 1
	CompletionItemKindMethod        = 2
	CompletionItemKindFunction      = 3
	CompletionItemKindConstructor   = 4
	CompletionItemKindField         = 5
	CompletionItemKindVariable      = 6
	CompletionItemKindClass         = 7
	CompletionItemKindInterface     = 8
	CompletionItemKindModule        = 9
	CompletionItemKindProperty      = 10
	CompletionItemKindUnit          = 11
	CompletionItemKindValue         = 12
	CompletionItemKindEnum          = 13
	CompletionItemKindKeyword       = 14
	CompletionItemKindSnippet       = 15
	CompletionItemKindColor         = 16
	CompletionItemKindFile          = 17
	CompletionItemKindReference     = 18
	CompletionItemKindFolder        = 19
	CompletionItemKindEnumMember    = 20
	CompletionItemKindConstant      = 21
	CompletionItemKindStruct        = 22
	CompletionItemKindEvent         = 23
	CompletionItemKindOperator      = 24
	CompletionItemKindTypeParameter = 25
)

// GML Language Server
type GMLLanguageServer struct {
	documents        map[string]string
	keywords         map[string]CompletionItem
	functions        map[string]CompletionItem
	constants        map[string]CompletionItem
	variables        map[string]CompletionItem
	tokenTypes       []string
	tokenModifiers   []string
	shutdownReceived bool
}

func NewGMLLanguageServer(rootPath string) *GMLLanguageServer {
	server := &GMLLanguageServer{
		documents: make(map[string]string),
		keywords:  make(map[string]CompletionItem),
		functions: make(map[string]CompletionItem),
		constants: make(map[string]CompletionItem),
		variables: make(map[string]CompletionItem),
		tokenTypes: []string{
			"namespace", "type", "class", "enum", "interface", "struct", "typeParameter",
			"parameter", "variable", "property", "enumMember", "event", "function",
			"method", "macro", "keyword", "modifier", "comment", "string", "number",
			"regexp", "operator",
		},
		tokenModifiers: []string{
			"declaration", "definition", "readonly", "static", "deprecated", "abstract",
			"async", "modification", "documentation", "defaultLibrary",
		},
		shutdownReceived: false,
	}
	server.initializeCompletions(rootPath)
	return server
}

func (s *GMLLanguageServer) scanDirectoryForGMLFiles(rootPath string) ([]string, error) {
	var gmlFiles []string
	err := filepath.Walk(rootPath, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if !info.IsDir() && filepath.Ext(path) == ".gml" {
			gmlFiles = append(gmlFiles, path)
		}
		return nil
	})
	return gmlFiles, err
}

var (
	varPattern      = regexp.MustCompile(`\bvar\s+([a-zA-Z_][a-zA-Z0-9_]*)\b`)
	funcPattern     = regexp.MustCompile(`\bfunction\s+([a-zA-Z_][a-zA-Z0-9_]*)\s*\(`)
	constantPattern = regexp.MustCompile(`(?:\b#macro\s+|\benum\s+)([a-zA-Z_][a-zA-Z0-9_]*)\b`)
)

func (s *GMLLanguageServer) parseGMLFile(filePath string) {
	file, err := os.Open(filePath)
	if err != nil {
		log.Printf("Error opening file %s: %v", filePath, err)
		return
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()

		// Extract variables
		varMatches := varPattern.FindAllStringSubmatch(line, -1)
		for _, match := range varMatches {
			if len(match) > 1 {
				variable := match[1]
				s.variables[variable] = CompletionItem{
					Label:  variable,
					Kind:   CompletionItemKindVariable,
					Detail: "User-defined variable",
				}
			}
		}

		// Extract functions
		funcMatches := funcPattern.FindAllStringSubmatch(line, -1)
		for _, match := range funcMatches {
			if len(match) > 1 {
				function := match[1]
				s.functions[function] = CompletionItem{
					Label:  function,
					Kind:   CompletionItemKindFunction,
					Detail: "User-defined function",
				}
			}
		}

		// Extract constants
		constMatches := constantPattern.FindAllStringSubmatch(line, -1)
		for _, match := range constMatches {
			if len(match) > 1 {
				constant := match[1]
				s.constants[constant] = CompletionItem{
					Label:  constant,
					Kind:   CompletionItemKindConstant,
					Detail: "User-defined constant",
				}
			}
		}
	}
	if err := scanner.Err(); err != nil {
		log.Printf("Error scanning file %s: %v", filePath, err)
	}
}

func (s *GMLLanguageServer) scanDirectoriesForAssets(rootPath string) map[string]CompletionItem {
	assetFolders := []string{"sprites", "scripts", "objects", "rooms", "tilesets", "sequences", "particle systems", "sounds", "shaders", "timelines", "fonts"}
	completions := make(map[string]CompletionItem)
	for _, folder := range assetFolders {
		fullPath := filepath.Join(rootPath, "..", "..", folder) // Adjust the path depth as necessary
		err := filepath.Walk(fullPath, func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return err
			}
			// Get the relative path from the asset folder
			relPath, err := filepath.Rel(fullPath, path)
			if err != nil {
				return err
			}
			// Clean up the path and replace separators for display purposes
			cleanPath := filepath.ToSlash(relPath)
			if info.IsDir() {
				// Add folder to completions
				completions[cleanPath] = CompletionItem{
					Label:  cleanPath,
					Kind:   CompletionItemKindFolder,
					Detail: fmt.Sprintf("Folder: %s", cleanPath),
				}
			} else {
				// Add file to completions
				completions[cleanPath] = CompletionItem{
					Label:  cleanPath,
					Kind:   CompletionItemKindFile,
					Detail: fmt.Sprintf("File: %s", cleanPath),
				}
			}
			return nil
		})
		if err != nil {
			log.Printf("Error scanning directory %s: %v", folder, err)
		}
	}
	return completions
}

func (s *GMLLanguageServer) initializeCompletions(rootPath string) {
	// Keywords
	keywords := []string{
		"case", "default", "while", "for", "do", "repeat", "until",
		"break", "return", "delete", "continue", "exit", "end", "new", "finally", "begin", "constructor",
		"throw", "try", "catch", "if", "else", "switch", "with", "function", "enum",
		"true", "false", "var", "globalvar", "static",
	}

	for _, keyword := range keywords {
		s.keywords[keyword] = CompletionItem{
			Label:  keyword,
			Kind:   CompletionItemKindKeyword,
			Detail: "GML keyword",
		}
	}

	// Built-in functions (parsed from the syntax file)
	functions := []string{
		// Game control
		"game_end", "game_restart", "game_load", "game_load_buffer", "game_save", "game_save_buffer", "game_get_speed", "game_set_speed",
		"highscore_add", "highscore_name", "highscore_value", "highscore_clear",

		// Movement and collision
		"place_empty", "place_free", "place_meeting", "position_empty", "position_meeting", "position_change", "position_destroy",
		"collision_circle", "collision_circle_list", "collision_line", "collision_line_list", "collision_point", "collision_point_list",
		"collision_ellipse", "collision_ellipse_list", "collision_rectangle", "collision_rectangle_list", "point_in_rectangle",
		"point_in_circle", "point_in_triangle", "rectangle_in_rectangle", "rectangle_in_circle", "rectangle_in_triangle",
		"motion_add", "motion_set", "move_towards_point", "move_bounce_all", "move_bounce_solid", "move_contact_all",
		"move_contact_solid", "move_outside_all", "move_outside_solid", "move_random", "move_snap", "move_wrap", "place_snapped",
		"mp_linear_step", "mp_linear_step_object", "mp_linear_path", "mp_potential_step", "mp_potential_step_object", "mp_potential_path",
		"mp_potential_path_object", "mp_potential_settings", "mp_grid_create", "mp_grid_destroy", "mp_grid_path", "mp_grid_add_cell",
		"mp_grid_add_instance", "mp_grid_add_rectangle", "mp_grid_get_cell", "mp_grid_clear_all", "mp_grid_clear_cell",
		"mp_grid_clear_rectangle", "mp_grid_to_ds_grid", "mp_grid_draw",

		// Math functions
		"random", "random_range", "irandom", "irandom_range", "random_set_seed", "random_get_seed", "randomize", "choose", "abs", "round", "floor", "ceil", "sign",
		"frac", "sqrt", "sqr", "exp", "ln", "log2", "log10", "sin", "cos", "tan", "arcsin", "arccos", "arctan", "arctan2", "dsin", "dcos", "dtan",
		"darcsin", "darccos", "darctan", "darctan2", "degtorad", "radtodeg", "power", "logn", "min", "max", "mean", "median", "clamp", "lerp",
		"dot_product", "dot_product_3d", "dot_product_normalised", "dot_product_3d_normalised", "math_set_epsilon", "math_get_epsilon",
		"angle_difference", "point_distance_3d", "point_distance", "point_direction", "lengthdir_x", "lengthdir_y",

		// String functions
		"real", "string", "string_format", "chr", "ansi_char", "ord", "string_length", "string_byte_length", "string_pos", "string_copy",
		"string_char_at", "string_ord_at", "string_byte_at", "string_set_byte_at", "string_delete", "string_insert", "string_lower", "string_upper",
		"string_repeat", "string_letters", "string_digits", "string_lettersdigits", "string_replace", "string_replace_all", "string_count",
		"clipboard_has_text", "clipboard_set_text", "clipboard_get_text",

		// Date functions
		"date_current_datetime", "date_create_datetime", "date_valid_datetime", "date_inc_year", "date_inc_month", "date_inc_week", "date_inc_day",
		"date_inc_hour", "date_inc_minute", "date_inc_second", "date_get_year", "date_get_month", "date_get_week", "date_get_day", "date_get_hour",
		"date_get_minute", "date_get_second", "date_get_weekday", "date_get_day_of_year", "date_get_hour_of_year", "date_get_minute_of_year",
		"date_get_second_of_year", "date_year_span", "date_month_span", "date_week_span", "date_day_span", "date_hour_span", "date_minute_span",
		"date_second_span", "date_compare_datetime", "date_compare_date", "date_compare_time", "date_date_of", "date_time_of",
		"date_datetime_string", "date_date_string", "date_time_string", "date_days_in_month", "date_days_in_year", "date_leap_year",
		"date_is_today", "date_set_timezone", "date_get_timezone",

		// Instance functions
		"distance_to_point", "distance_to_object", "event_perform", "event_user", "event_perform_object", "event_inherited",
		"show_debug_message", "show_debug_overlay", "debug_event", "alarm_get", "alarm_set",
		"variable_global_exists", "variable_global_get", "variable_global_set",

		// Debug functions
		"dbg_view", "dbg_section", "dbg_text", "dbg_slider", "dbg_slider_int",
		"dbg_button", "dbg_checkbox", "dbg_color", "dbg_colour", "dbg_same_line",
		"dbg_drop_down_menu", "dbg_text_input", "dbg_multi_text_input",
		"dbg_watch", "dbg_sprite", "dbg_surface",

		// Flex panel

		"flexpanel_create_node",
		"flexpanel_delete_node",
		"flexpanel_node_insert_child",
		"flexpanel_node_remove_child",
		"flexpanel_node_remove_all_children",
		"flexpanel_calculate_layout",

		// Node properties
		"flexpanel_node_set_name",
		"flexpanel_node_get_name",
		"flexpanel_node_get_num_children",
		"flexpanel_node_get_child",
		"flexpanel_node_get_child_hash",
		"flexpanel_get_child_has_name",
		"flexpanel_node_get_parent",
		"flexpanel_node_get_data",
		"flexpanel_node_get_struct",
		"flexpanel_node_layout_get_position",

		// Measure
		"flexpanel_node_set_measure_function",
		"flexpanel_node_get_measure_function",

		// Style: Size
		"flexpanel_node_style_set_width",
		"flexpanel_node_style_get_width",
		"flexpanel_node_style_set_height",
		"flexpanel_node_style_get_height",
		"flexpanel_node_style_set_min_width",
		"flexpanel_node_style_get_min_width",
		"flexpanel_node_style_set_max_width",
		"flexpanel_node_style_get_max_width",
		"flexpanel_node_style_set_min_height",
		"flexpanel_node_style_get_min_height",
		"flexpanel_node_style_set_max_height",
		"flexpanel_node_style_get_max_height",
		"flexpanel_node_style_set_aspect_ratio",
		"flexpanel_node_style_get_aspect_ratio",

		// Style: Position
		"flexpanel_node_style_set_position",
		"flexpanel_node_style_get_position",
		"flexpanel_node_style_set_position_type",
		"flexpanel_node_style_get_position_type",

		// Style: Spacing
		"flexpanel_node_style_set_margin",
		"flexpanel_node_style_get_margin",
		"flexpanel_node_style_set_padding",
		"flexpanel_node_style_get_padding",
		"flexpanel_node_style_set_border",
		"flexpanel_node_style_get_border",
		"flexpanel_node_style_set_gap",
		"flexpanel_node_style_get_gap",

		// Style: Flex
		"flexpanel_node_style_set_direction",
		"flexpanel_node_style_get_direction",
		"flexpanel_node_style_set_flex_direction",
		"flexpanel_node_style_get_flex_direction",
		"flexpanel_node_style_set_flex_wrap",
		"flexpanel_node_style_get_flex_wrap",
		"flexpanel_node_style_set_flex_basis",
		"flexpanel_node_style_get_flex_basis",
		"flexpanel_node_style_set_flex_grow",
		"flexpanel_node_style_get_flex_grow",
		"flexpanel_node_style_set_flex_shrink",
		"flexpanel_node_style_get_flex_shrink",
		"flexpanel_node_style_set_flex",
		"flexpanel_node_style_get_flex",

		// Style: Alignment
		"flexpanel_node_style_set_justify_content",
		"flexpanel_node_style_get_justify_content",
		"flexpanel_node_style_set_align_items",
		"flexpanel_node_style_get_align_items",
		"flexpanel_node_style_set_align_content",
		"flexpanel_node_style_get_align_content",
		"flexpanel_node_style_set_align_self",
		"flexpanel_node_style_get_align_self",

		// Style: Display
		"flexpanel_node_style_set_display",
		"flexpanel_node_style_get_display",

		// Style: Overflow and Clipping
		"flexpanel_node_style_set_overflow",
		"flexpanel_node_style_get_overflow",
		"flexpanel_node_style_set_clip_content",
		"flexpanel_node_style_get_clip_content",

		// Input functions
		"keyboard_set_map", "keyboard_get_map", "keyboard_unset_map", "keyboard_check", "keyboard_check_pressed", "keyboard_check_released",
		"keyboard_check_direct", "keyboard_get_numlock", "keyboard_set_numlock", "keyboard_key_press", "keyboard_key_release", "keyboard_clear",
		"io_clear", "browser_input_capture", "mouse_check_button", "mouse_check_button_pressed", "mouse_check_button_released", "mouse_wheel_up",
		"mouse_wheel_down", "mouse_clear", "keyboard_virtual_hide", "keyboard_virtual_show",
		"keyboard_virtual_status",

		// Drawing functions
		"draw_self", "draw_sprite", "draw_sprite_pos", "draw_sprite_ext", "draw_sprite_stretched",
		"draw_sprite_stretched_ext", "draw_sprite_tiled", "draw_sprite_tiled_ext", "draw_sprite_part", "draw_sprite_part_ext", "draw_sprite_general",
		"draw_clear", "draw_clear_alpha", "draw_point", "draw_line", "draw_line_width", "draw_rectangle", "draw_roundrect", "draw_roundrect_ext",
		"draw_triangle", "draw_circle", "draw_ellipse", "draw_set_circle_precision", "draw_arrow", "draw_button", "draw_path", "draw_healthbar",
		"draw_getpixel", "draw_getpixel_ext", "draw_set_colour", "draw_set_color", "draw_set_alpha", "draw_get_colour", "draw_get_color",
		"draw_get_alpha", "make_colour_rgb", "make_colour_hsv", "colour_get_red", "colour_get_green", "colour_get_blue", "colour_get_hue",
		"colour_get_saturation", "colour_get_value", "merge_colour", "make_color_rgb", "make_color_hsv", "color_get_red", "color_get_green",
		"color_get_blue", "color_get_hue", "color_get_saturation", "color_get_value", "merge_color", "screen_save", "screen_save_part", "gif_open",
		"gif_add_surface", "gif_save", "draw_set_font", "draw_set_halign", "draw_set_valign", "draw_text", "draw_text_ext", "string_width",
		"string_height", "string_width_ext", "string_height_ext", "draw_text_transformed", "draw_text_ext_transformed", "draw_text_colour",
		"draw_text_ext_colour", "draw_text_transformed_colour", "draw_text_ext_transformed_colour", "draw_text_color", "draw_text_ext_color",
		"draw_text_transformed_color", "draw_text_ext_transformed_color", "draw_point_colour", "draw_line_colour", "draw_line_width_colour",
		"draw_rectangle_colour", "draw_roundrect_colour", "draw_roundrect_colour_ext", "draw_triangle_colour", "draw_circle_colour",
		"draw_ellipse_colour", "draw_point_color", "draw_line_color", "draw_line_width_color", "draw_rectangle_color", "draw_roundrect_color",
		"draw_roundrect_color_ext", "draw_triangle_color", "draw_circle_color", "draw_ellipse_color", "draw_primitive_begin", "draw_vertex",
		"draw_vertex_colour", "draw_vertex_color", "draw_vertex_argb", "draw_vertex_texcoord", "draw_vertex_normal", "draw_vertex_float1",
		"draw_vertex_float2", "draw_vertex_float3", "draw_vertex_float4", "draw_vertex_ubyte4", "draw_primitive_end", "sprite_get_uvs",
		"font_get_uvs", "sprite_get_texture", "font_get_texture", "texture_get_width", "texture_get_height", "draw_primitive_begin_texture",
		"draw_vertex_texture", "draw_vertex_texture_colour", "draw_vertex_texture_color", "texture_global_scale",

		// Surface functions
		"surface_create", "surface_create_ext", "surface_resize", "surface_free", "surface_exists", "surface_get_width", "surface_get_height", "surface_get_texture",
		"surface_set_target", "surface_set_target_ext", "surface_reset_target", "draw_surface", "draw_surface_stretched", "draw_surface_tiled",
		"draw_surface_part", "draw_surface_ext", "draw_surface_stretched_ext", "draw_surface_tiled_ext", "draw_surface_part_ext",
		"draw_surface_general", "surface_getpixel", "surface_getpixel_ext", "surface_save", "surface_save_part", "surface_copy", "surface_copy_part",
		"application_surface_draw_enable", "application_get_position", "application_surface_enable", "application_surface_is_enabled",

		// Display and window functions
		"display_get_width", "display_get_height", "display_get_frequency", "display_get_orientation", "display_get_gui_width", "display_get_gui_height",
		"display_reset", "display_mouse_get_x", "display_mouse_get_y", "display_mouse_set", "display_set_ui_visibility", "window_set_fullscreen",
		"window_get_fullscreen", "window_set_caption", "window_set_min_width", "window_set_max_width", "window_set_min_height",
		"window_set_max_height", "window_get_visible_rects", "window_get_caption", "window_set_cursor", "window_get_cursor", "window_set_colour",
		"window_get_colour", "window_set_color", "window_get_color", "window_set_position", "window_set_size", "window_set_rectangle",
		"window_center", "window_get_x", "window_get_y", "window_get_width", "window_get_height", "window_mouse_get_x", "window_mouse_get_y",
		"window_mouse_set", "window_view_mouse_get_x", "window_view_mouse_get_y", "window_views_mouse_get_x", "window_views_mouse_get_y",

		// Dialog functions
		"show_message", "show_message_async", "clickable_add", "clickable_add_ext", "clickable_change", "clickable_change_ext", "clickable_delete",
		"clickable_exists", "clickable_set_style", "show_question", "show_question_async", "get_integer", "get_string", "get_integer_async",
		"get_string_async", "get_login_async", "get_open_filename", "get_save_filename", "get_open_filename_ext", "get_save_filename_ext",
		"show_error", "draw_highscore",

		// Timeline functions
		"timeline_exists", "timeline_get_name", "timeline_moment_clear", "timeline_moment_add_script",
		"timeline_size", "timeline_max_moment", "timeline_add", "timeline_delete", "timeline_clear",

		// Room functions
		"room_set_background_colour", "room_set_background_color",

		// File I/O functions
		"file_text_open_from_string", "file_text_open_read", "file_text_open_write", "file_text_open_append",
		"file_text_close", "file_text_write_string", "file_text_write_real", "file_text_writeln", "file_text_read_string", "file_text_read_real",
		"file_text_readln", "file_text_eof", "file_text_eoln", "file_exists", "file_delete", "file_rename", "file_copy", "directory_exists",
		"directory_create", "directory_destroy", "file_find_first", "file_find_next", "file_find_close", "file_attributes", "filename_name",
		"filename_path", "filename_dir", "filename_drive", "filename_ext", "filename_change_ext", "file_bin_open", "file_bin_rewrite",
		"file_bin_close", "file_bin_position", "file_bin_size", "file_bin_seek", "file_bin_write_byte", "file_bin_read_byte", "parameter_count",
		"parameter_string", "environment_get_variable",

		// INI functions
		"ini_open_from_string", "ini_open", "ini_close", "ini_read_string", "ini_read_real",
		"ini_write_string", "ini_write_real", "ini_key_exists", "ini_section_exists", "ini_key_delete", "ini_section_delete",

		// Data structures functions
		"ds_set_precision", "ds_exists",
		"ds_stack_create", "ds_stack_destroy", "ds_stack_clear", "ds_stack_copy", "ds_stack_size", "ds_stack_empty", "ds_stack_push",
		"ds_stack_pop", "ds_stack_top", "ds_stack_write", "ds_stack_read",
		"ds_queue_create", "ds_queue_destroy", "ds_queue_clear", "ds_queue_copy", "ds_queue_size", "ds_queue_empty", "ds_queue_enqueue",
		"ds_queue_dequeue", "ds_queue_head", "ds_queue_tail", "ds_queue_write", "ds_queue_read",
		"ds_list_create", "ds_list_destroy", "ds_list_clear", "ds_list_copy", "ds_list_size", "ds_list_empty", "ds_list_add", "ds_list_insert",
		"ds_list_replace", "ds_list_delete", "ds_list_find_index", "ds_list_find_value", "ds_list_mark_as_list", "ds_list_mark_as_map",
		"ds_list_sort", "ds_list_shuffle", "ds_list_write", "ds_list_read",
		"ds_map_create", "ds_map_destroy", "ds_map_clear", "ds_map_copy", "ds_map_size", "ds_map_empty", "ds_map_add", "ds_map_add_list",
		"ds_map_add_map", "ds_map_replace", "ds_map_replace_map", "ds_map_replace_list", "ds_map_delete", "ds_map_exists", "ds_map_find_value",
		"ds_map_find_previous", "ds_map_find_next", "ds_map_find_first", "ds_map_find_last", "ds_map_write", "ds_map_read", "ds_map_secure_save",
		"ds_map_secure_load", "ds_map_secure_load_buffer", "ds_map_secure_save_buffer",
		"ds_priority_create", "ds_priority_destroy", "ds_priority_clear", "ds_priority_copy", "ds_priority_size", "ds_priority_empty",
		"ds_priority_add", "ds_priority_change_priority", "ds_priority_find_priority", "ds_priority_delete_value", "ds_priority_delete_min",
		"ds_priority_find_min", "ds_priority_delete_max", "ds_priority_find_max", "ds_priority_write", "ds_priority_read",
		"ds_grid_create", "ds_grid_destroy", "ds_grid_copy", "ds_grid_resize", "ds_grid_width", "ds_grid_height", "ds_grid_clear", "ds_grid_set", "ds_grid_add",
		"ds_grid_multiply", "ds_grid_set_region", "ds_grid_add_region", "ds_grid_multiply_region", "ds_grid_set_disk", "ds_grid_add_disk",
		"ds_grid_multiply_disk", "ds_grid_set_grid_region", "ds_grid_add_grid_region", "ds_grid_multiply_grid_region", "ds_grid_get", "ds_grid_get_sum",
		"ds_grid_get_max", "ds_grid_get_min", "ds_grid_get_mean", "ds_grid_get_disk_sum", "ds_grid_get_disk_min", "ds_grid_get_disk_max",
		"ds_grid_get_disk_mean", "ds_grid_value_exists", "ds_grid_value_x", "ds_grid_value_y", "ds_grid_value_disk_exists", "ds_grid_value_disk_x",
		"ds_grid_value_disk_y", "ds_grid_shuffle", "ds_grid_write", "ds_grid_read", "ds_grid_sort",

		// Effects and particles
		"effect_create_below", "effect_create_above", "effect_clear",
		"part_type_create", "part_type_destroy", "part_type_exists", "part_type_clear", "part_type_shape", "part_type_sprite",
		"part_type_size", "part_type_scale", "part_type_orientation", "part_type_life", "part_type_step", "part_type_death", "part_type_speed",
		"part_type_direction", "part_type_gravity", "part_type_colour1", "part_type_colour2", "part_type_colour3", "part_type_colour_mix",
		"part_type_colour_rgb", "part_type_colour_hsv", "part_type_color1", "part_type_color2", "part_type_color3", "part_type_color_mix",
		"part_type_color_rgb", "part_type_color_hsv", "part_type_alpha1", "part_type_alpha2", "part_type_alpha3", "part_type_blend",
		"part_system_create", "part_system_destroy", "part_system_exists", "part_system_clear", "part_system_draw_order", "part_system_depth",
		"part_system_position", "part_system_automatic_update", "part_system_automatic_draw", "part_system_update", "part_system_drawit",
		"part_particles_create", "part_particles_create_colour", "part_particles_create_color", "part_particles_clear", "part_particles_count",
		"part_emitter_create", "part_emitter_destroy", "part_emitter_destroy_all", "part_emitter_exists", "part_emitter_clear", "part_emitter_region",
		"part_emitter_burst", "part_emitter_stream",

		// External functions
		"external_call", "external_define", "external_free", "window_handle", "window_device",
		"matrix_get", "matrix_set", "matrix_build", "matrix_multiply",
		"os_get_config", "os_get_info", "os_get_language", "os_get_region", "os_lock_orientation",
		"display_get_dpi_x", "display_get_dpi_y", "display_set_gui_size", "display_set_gui_maximise", "device_mouse_dbclick_enable",
		"virtual_key_add", "virtual_key_hide", "virtual_key_delete", "virtual_key_show", "draw_enable_drawevent", "draw_enable_swf_aa",
		"draw_set_swf_aa_level", "draw_get_swf_aa_level", "draw_texture_flush", "draw_flush", "shop_leave_rating", "url_get_domain", "url_open",
		"url_open_ext", "url_open_full", "get_timer",

		// Achievement functions
		"achievement_login", "achievement_logout", "achievement_post", "achievement_increment",
		"achievement_post_score", "achievement_available", "achievement_show_achievements", "achievement_show_leaderboards", "achievement_load_friends",
		"achievement_load_leaderboard", "achievement_send_challenge", "achievement_load_progress", "achievement_reset", "achievement_login_status",
		"achievement_get_pic", "achievement_show_challenge_notifications", "achievement_get_challenges", "achievement_event", "achievement_show",
		"achievement_get_info",

		// Physics functions
		"physics_world_create", "physics_world_gravity", "physics_world_update_speed", "physics_world_update_iterations",
		"physics_world_draw_debug", "physics_pause_enable", "physics_fixture_create", "physics_fixture_set_kinematic", "physics_fixture_set_density",
		"physics_fixture_set_awake", "physics_fixture_set_restitution", "physics_fixture_set_friction", "physics_fixture_set_collision_group",
		"physics_fixture_set_sensor", "physics_fixture_set_linear_damping", "physics_fixture_set_angular_damping", "physics_fixture_set_circle_shape",
		"physics_fixture_set_box_shape", "physics_fixture_set_edge_shape", "physics_fixture_set_polygon_shape", "physics_fixture_set_chain_shape",
		"physics_fixture_add_point", "physics_fixture_bind", "physics_fixture_bind_ext", "physics_fixture_delete", "physics_apply_force",
		"physics_apply_impulse", "physics_apply_angular_impulse", "physics_apply_local_force", "physics_apply_local_impulse", "physics_apply_torque",
		"physics_mass_properties", "physics_draw_debug", "physics_test_overlap", "physics_remove_fixture", "physics_set_friction", "physics_set_density",
		"physics_set_restitution", "physics_get_friction", "physics_get_density", "physics_get_restitution", "physics_joint_distance_create",
		"physics_joint_rope_create", "physics_joint_revolute_create", "physics_joint_prismatic_create", "physics_joint_pulley_create",
		"physics_joint_wheel_create", "physics_joint_weld_create", "physics_joint_friction_create", "physics_joint_gear_create",
		"physics_joint_enable_motor", "physics_joint_get_value", "physics_joint_set_value", "physics_joint_delete", "physics_particle_create",
		"physics_particle_delete", "physics_particle_delete_region_circle", "physics_particle_delete_region_box", "physics_particle_delete_region_poly",
		"physics_particle_set_flags", "physics_particle_set_category_flags", "physics_particle_draw", "physics_particle_draw_ext",
		"physics_particle_count", "physics_particle_get_data", "physics_particle_get_data_particle", "physics_particle_group_begin",
		"physics_particle_group_circle", "physics_particle_group_box", "physics_particle_group_polygon", "physics_particle_group_add_point",
		"physics_particle_group_end", "physics_particle_group_join", "physics_particle_group_delete", "physics_particle_group_count",
		"physics_particle_group_get_data", "physics_particle_group_get_mass", "physics_particle_group_get_inertia", "physics_particle_group_get_centre_x",
		"physics_particle_group_get_centre_y", "physics_particle_group_get_vel_x", "physics_particle_group_get_vel_y", "physics_particle_group_get_ang_vel",
		"physics_particle_group_get_x", "physics_particle_group_get_y", "physics_particle_group_get_angle", "physics_particle_set_group_flags",
		"physics_particle_get_group_flags", "physics_particle_get_max_count", "physics_particle_get_radius", "physics_particle_get_density",
		"physics_particle_get_damping", "physics_particle_get_gravity_scale", "physics_particle_set_max_count", "physics_particle_set_radius",
		"physics_particle_set_density", "physics_particle_set_damping", "physics_particle_set_gravity_scale",

		// Cloud and device functions
		"cloud_file_save", "cloud_string_save", "cloud_synchronise",
		"device_get_tilt_x", "device_get_tilt_y", "device_get_tilt_z", "device_is_keypad_open", "device_mouse_check_button",
		"device_mouse_check_button_pressed", "device_mouse_check_button_released",

		// In-app purchase functions
		"iap_activate", "iap_status", "iap_enumerate_products", "iap_restore_all",
		"iap_acquire", "iap_consume", "iap_product_details", "iap_purchase_details",

		// Gamepad functions
		"gamepad_is_supported", "gamepad_get_device_count", "gamepad_is_connected", "gamepad_get_description", "gamepad_get_button_threshold",
		"gamepad_set_button_threshold", "gamepad_get_axis_deadzone", "gamepad_set_axis_deadzone", "gamepad_button_count", "gamepad_button_check",
		"gamepad_button_check_pressed", "gamepad_button_check_released", "gamepad_button_value", "gamepad_axis_count", "gamepad_axis_value",
		"gamepad_set_vibration", "gamepad_set_color", "gamepad_set_colour",

		// System functions
		"os_is_paused", "window_has_focus", "code_is_compiled",
		"http_get", "http_get_file", "http_post_string", "http_request", "json_encode", "json_decode", "json_parse", "json_stringify",
		"zip_unzip", "base64_encode", "base64_decode", "md5_string_unicode", "md5_string_utf8", "md5_file", "os_is_network_connected", "sha1_string_unicode",
		"sha1_string_utf8", "sha1_file", "os_powersave_enable", "analytics_event", "analytics_event_ext",

		// Windows 8 functions
		"win8_livetile_tile_notification", "win8_livetile_tile_clear", "win8_livetile_badge_notification", "win8_livetile_badge_clear", "win8_livetile_queue_enable",
		"win8_secondarytile_pin", "win8_secondarytile_badge_notification", "win8_secondarytile_delete", "win8_livetile_notification_begin", "win8_livetile_notification_secondary_begin",
		"win8_livetile_notification_expiry", "win8_livetile_notification_tag", "win8_livetile_notification_text_add", "win8_livetile_notification_image_add",
		"win8_livetile_notification_end", "win8_appbar_enable", "win8_appbar_add_element", "win8_appbar_remove_element", "win8_settingscharm_add_entry",
		"win8_settingscharm_add_html_entry", "win8_settingscharm_add_xaml_entry", "win8_settingscharm_set_xaml_property", "win8_settingscharm_get_xaml_property",
		"win8_settingscharm_remove_entry", "win8_share_image", "win8_share_screenshot", "win8_share_file", "win8_share_url", "win8_share_text", "win8_search_enable",
		"win8_search_disable", "win8_search_add_suggestions", "win8_device_touchscreen_available", "win8_license_initialize_sandbox", "win8_license_trial_version",

		// Windows Phone functions
		"winphone_license_trial_version", "winphone_tile_title", "winphone_tile_count", "winphone_tile_back_title", "winphone_tile_back_content",
		"winphone_tile_back_content_wide", "winphone_tile_front_image", "winphone_tile_front_image_small", "winphone_tile_front_image_wide", "winphone_tile_back_image",
		"winphone_tile_back_image_wide", "winphone_tile_background_colour", "winphone_tile_background_color", "winphone_tile_icon_image", "winphone_tile_small_icon_image",
		"winphone_tile_wide_content", "winphone_tile_cycle_images", "winphone_tile_small_background_image",

		// Networking functions
		"network_create_socket", "network_create_socket_ext", "network_create_server", "network_create_server_raw", "network_connect",
		"network_connect_raw", "network_send_packet", "network_send_raw", "network_send_broadcast", "network_send_udp", "network_send_udp_raw",
		"network_set_timeout", "network_set_config", "network_resolve", "network_destroy",

		// Miscellaneous functions
		"gml_release_mode", "gml_pragma",

		// Steam functions
		"steam_activate_overlay", "steam_is_overlay_enabled", "steam_is_overlay_activated", "steam_get_persona_name", "steam_initialised",
		"steam_is_cloud_enabled_for_app", "steam_is_cloud_enabled_for_account", "steam_file_persisted", "steam_get_quota_total", "steam_get_quota_free", "steam_file_write",
		"steam_file_write_file", "steam_file_read", "steam_file_delete", "steam_file_exists", "steam_file_size", "steam_file_share", "steam_is_screenshot_requested",
		"steam_send_screenshot", "steam_is_user_logged_on", "steam_get_user_steam_id", "steam_user_owns_dlc", "steam_user_installed_dlc", "steam_set_achievement",
		"steam_get_achievement", "steam_clear_achievement", "steam_set_stat_int", "steam_set_stat_float", "steam_set_stat_avg_rate", "steam_get_stat_int", "steam_get_stat_float",
		"steam_get_stat_avg_rate", "steam_reset_all_stats", "steam_reset_all_stats_achievements", "steam_stats_ready", "steam_create_leaderboard", "steam_upload_score",
		"steam_upload_score_ext", "steam_download_scores_around_user", "steam_download_scores", "steam_download_friends_scores", "steam_upload_score_buffer",
		"steam_upload_score_buffer_ext", "steam_current_game_language", "steam_available_languages", "steam_activate_overlay_browser", "steam_activate_overlay_user",
		"steam_activate_overlay_store", "steam_get_user_persona_name", "steam_get_app_id", "steam_get_user_account_id", "steam_ugc_download", "steam_ugc_create_item",
		"steam_ugc_start_item_update", "steam_ugc_set_item_title", "steam_ugc_set_item_description", "steam_ugc_set_item_visibility", "steam_ugc_set_item_tags",
		"steam_ugc_set_item_content", "steam_ugc_set_item_preview", "steam_ugc_submit_item_update", "steam_ugc_get_item_update_progress", "steam_ugc_subscribe_item",
		"steam_ugc_unsubscribe_item", "steam_ugc_num_subscribed_items", "steam_ugc_get_subscribed_items", "steam_ugc_get_item_install_info", "steam_ugc_get_item_update_info",
		"steam_ugc_request_item_details", "steam_ugc_create_query_user", "steam_ugc_create_query_user_ex", "steam_ugc_create_query_all", "steam_ugc_create_query_all_ex",
		"steam_ugc_query_set_cloud_filename_filter", "steam_ugc_query_set_match_any_tag", "steam_ugc_query_set_search_text", "steam_ugc_query_set_ranked_by_trend_days",
		"steam_ugc_query_add_required_tag", "steam_ugc_query_add_excluded_tag", "steam_ugc_query_set_return_long_description", "steam_ugc_query_set_return_total_only",
		"steam_ugc_query_set_allow_cached_response", "steam_ugc_send_query",

		// Vertex buffer functions
		"vertex_format_begin", "vertex_format_end", "vertex_format_delete", "vertex_format_add_position",
		"vertex_format_add_position_3d", "vertex_format_add_colour", "vertex_format_add_color", "vertex_format_add_normal", "vertex_format_add_textcoord",
		"vertex_format_add_custom", "vertex_create_buffer", "vertex_create_buffer_ext", "vertex_delete_buffer", "vertex_begin", "vertex_end", "vertex_position",
		"vertex_position_3d", "vertex_colour", "vertex_color", "vertex_argb", "vertex_texcoord", "vertex_normal", "vertex_float1", "vertex_float2", "vertex_float3",
		"vertex_float4", "vertex_ubyte4", "vertex_submit", "vertex_freeze", "vertex_get_number", "vertex_get_buffer_size", "vertex_create_buffer_from_buffer",
		"vertex_create_buffer_from_buffer_ext",

		// Push notification functions
		"push_local_notification", "push_get_first_local_notification", "push_get_next_local_notification", "push_cancel_local_notification",

		// Buffer functions
		"buffer_exists", "buffer_create", "buffer_create_from_vertex_buffer", "buffer_create_from_vertex_buffer_ext", "buffer_delete", "buffer_read", "buffer_fill",
		"buffer_write", "buffer_seek", "buffer_tell", "buffer_peek", "buffer_poke", "buffer_save", "buffer_save_ext", "buffer_save_async", "buffer_load",
		"buffer_load_ext", "buffer_load_async", "buffer_load_partial", "buffer_compress", "buffer_decompress", "buffer_async_group_begin", "buffer_async_group_option",
		"buffer_async_group_end", "buffer_copy", "buffer_copy_from_vertex_buffer", "buffer_get_type", "buffer_get_alignment", "buffer_get_address", "buffer_get_size",
		"buffer_get_surface", "buffer_set_surface", "buffer_resize", "buffer_sizeof", "buffer_md5", "buffer_sha1", "buffer_crc32", "buffer_base64_encode",
		"buffer_base64_decode", "buffer_base64_decode_ext", "buffer_set_used_size",

		// Instance functions (additional)
		"instance_activate_all", "instance_activate_object", "instance_activate_region",
		"instance_change", "instance_copy", "instance_create", "instance_create_depth",
		"instance_create_layer", "instance_deactivate_all", "instance_deactivate_object",
		"instance_deactivate_region", "instance_destroy", "instance_exists", "instance_find",
		"instance_furthest", "instance_id_get", "instance_nearest", "instance_number",
		"instance_place", "instance_place_list", "instance_position", "instance_position_list",

		// Layer functions
		"layer_add_instance", "layer_background_alpha", "layer_background_blend", "layer_background_change",
		"layer_background_create", "layer_background_destroy", "layer_background_exists",
		"layer_background_get_alpha", "layer_background_get_blend", "layer_background_get_id",
		"layer_background_get_index", "layer_background_get_speed", "layer_background_get_visible",
		"layer_background_htiled", "layer_background_index", "layer_background_set_alpha",
		"layer_background_set_blend", "layer_background_set_index", "layer_background_set_speed",
		"layer_background_set_visible", "layer_background_speed", "layer_background_visible",
		"layer_background_vtiled", "layer_background_xscale", "layer_background_yscale",
		"layer_create", "layer_depth", "layer_destroy", "layer_destroy_instances",
		"layer_element_move", "layer_exists", "layer_force_draw_depth", "layer_get_all",
		"layer_get_all_elements", "layer_get_depth", "layer_get_element_layer", "layer_get_element_type",
		"layer_get_hspeed", "layer_get_id", "layer_get_id_at_depth", "layer_get_name",
		"layer_get_script_begin", "layer_get_script_end", "layer_get_shader", "layer_get_target_room",
		"layer_get_visible", "layer_get_vspeed", "layer_get_x", "layer_get_y", "layer_has_instance",
		"layer_hspeed", "layer_instance_get_instance", "layer_is_draw_depth_forced",
		"layer_move", "layer_reset_target_room", "layer_script_begin", "layer_script_end",
		"layer_set_target_room", "layer_set_visible", "layer_shader", "layer_sprite_alpha",
		"layer_sprite_angle", "layer_sprite_blend", "layer_sprite_change", "layer_sprite_create",
		"layer_sprite_destroy", "layer_sprite_exists", "layer_sprite_get_alpha", "layer_sprite_get_angle",
		"layer_sprite_get_blend", "layer_sprite_get_id", "layer_sprite_get_index", "layer_sprite_get_speed",
		"layer_sprite_get_visible", "layer_sprite_get_x", "layer_sprite_get_xscale", "layer_sprite_get_y",
		"layer_sprite_get_yscale", "layer_sprite_index", "layer_sprite_set_alpha", "layer_sprite_set_angle",
		"layer_sprite_set_blend", "layer_sprite_set_index", "layer_sprite_set_speed",
		"layer_sprite_set_visible", "layer_sprite_speed", "layer_sprite_visible", "layer_sprite_x",
		"layer_sprite_xscale", "layer_sprite_y", "layer_sprite_yscale", "layer_tilemap_create",
		"layer_tilemap_destroy", "layer_tilemap_exists", "layer_tilemap_get_id", "layer_tile_change",
		"layer_tile_exists", "layer_tile_get_empty", "layer_tile_region", "layer_vspeed",
		"layer_x", "layer_y",

		// Object functions
		"object_exists", "object_get_name", "object_get_depth", "object_get_mask",
		"object_get_parent", "object_get_persistent", "object_get_physics", "object_get_solid",
		"object_get_sprite", "object_get_visible", "object_is_ancestor", "object_set_depth",
		"object_set_mask", "object_set_persistent", "object_set_solid", "object_set_sprite",
		"object_set_visible",

		// Operating system functions
		"os_get_config", "os_get_info", "os_get_language", "os_get_region",

		// Path functions
		"path_exists", "path_get_name", "path_get_length", "path_get_time", "path_get_kind",
		"path_get_closed", "path_get_precision", "path_get_number", "path_get_point_x",
		"path_get_point_y", "path_get_point_speed", "path_get_x", "path_get_y", "path_get_speed",
		"path_create", "path_assign", "path_duplicate", "path_append", "path_delete", "path_add",
		"path_add_point", "path_insert_point", "path_change_point", "path_delete_point",
		"path_clear_points", "path_reverse", "path_mirror", "path_flip", "path_rotate",
		"path_rescale", "path_shift", "path_set_kind", "path_set_closed", "path_set_precision",

		// Room functions
		"room_exists", "room_get_name", "room_assign", "room_duplicate", "room_goto",
		"room_goto_next", "room_goto_previous", "room_next", "room_previous", "room_restart",
		"room_set_height", "room_set_width", "room_set_persistent", "room_get_camera",
		"room_set_camera", "room_get_viewport", "room_set_viewport", "room_set_view",
		"room_set_view_enabled", "room_add", "room_instance_add", "room_instance_clear",
		"room_tile_add", "room_tile_add_ext", "room_tile_clear",

		// Script functions
		"script_exists", "script_get_name", "script_execute", "script_execute_ext",

		// Shader functions
		"shader_current", "shader_exists", "shader_get_name", "shader_get_sampler_index",
		"shader_get_uniform", "shader_is_compiled", "shader_reset", "shader_set",
		"shader_set_uniform_f", "shader_set_uniform_f_array", "shader_set_uniform_i",
		"shader_set_uniform_i_array", "shader_set_uniform_matrix", "shader_set_uniform_matrix_array",
		"shaders_are_supported", "shader_enable_corner_id",

		// Sprite functions (comprehensive)
		"sprite_exists", "sprite_get_name", "sprite_get_number", "sprite_get_width", "sprite_get_height",
		"sprite_get_xoffset", "sprite_get_yoffset", "sprite_get_bbox_left", "sprite_get_bbox_right",
		"sprite_get_bbox_top", "sprite_get_bbox_bottom", "sprite_get_bbox_mode", "sprite_get_precise",
		"sprite_get_texture", "sprite_get_uvs", "sprite_get_speed", "sprite_get_speed_type",
		"sprite_set_offset", "sprite_set_bbox_mode", "sprite_set_bbox", "sprite_set_precise",
		"sprite_set_alpha_from_sprite", "sprite_set_cache_size", "sprite_set_cache_size_ext",
		"sprite_set_speed", "sprite_create_from_surface", "sprite_add_from_surface", "sprite_add",
		"sprite_replace", "sprite_duplicate", "sprite_assign", "sprite_merge", "sprite_save",
		"sprite_save_strip", "sprite_delete", "sprite_collision_mask", "sprite_nineslice_create",
		"sprite_nineslice_update",

		// String functions (additional)
		"string_hash_to_newline", "string_width_ext_transformed", "string_height_ext_transformed",
		"string_ext", "string_trim", "string_starts_with", "string_ends_with",

		// Texture functions
		"texture_get_texel_width", "texture_get_texel_height", "texture_set_stage",
		"texture_get_uvs", "texture_is_ready", "texture_prefetch", "texture_flush", "texture_group_load",
		"texture_group_unload", "texture_group_is_loaded", "texture_set_interpolation",
		"texture_set_blending", "texture_set_repeat", "texture_set_border_colour",
		"texture_set_border_color", "texturegroup_get_sprites", "texturegroup_get_textures",

		// Tilemap functions
		"tilemap_clear", "tilemap_copy", "tilemap_create", "tilemap_delete", "tilemap_exists",
		"tilemap_get", "tilemap_get_at_pixel", "tilemap_get_cell_x_at_pixel", "tilemap_get_cell_y_at_pixel",
		"tilemap_get_frame", "tilemap_get_global_mask", "tilemap_get_height", "tilemap_get_mask",
		"tilemap_get_pixel_height", "tilemap_get_pixel_width", "tilemap_get_tile_height",
		"tilemap_get_tile_width", "tilemap_get_tileset", "tilemap_get_width", "tilemap_get_x",
		"tilemap_get_y", "tilemap_set", "tilemap_set_at_pixel", "tilemap_set_global_mask",
		"tilemap_set_mask", "tilemap_tileset", "tilemap_x", "tilemap_y",

		// Tileset functions
		"tileset_get_info", "tileset_get_name", "tileset_get_texture", "tileset_get_uvs",

		// Time source functions (scheduler system)
		"time_source_create", "time_source_destroy", "time_source_start", "time_source_stop",
		"time_source_pause", "time_source_resume", "time_source_reconfigure", "time_source_reset",
		"time_source_get_children", "time_source_get_parent", "time_source_get_period",
		"time_source_get_reps_completed", "time_source_get_reps_remaining", "time_source_get_state",
		"time_source_get_time_remaining", "time_source_get_units", "time_source_exists",

		// Video functions
		"video_open", "video_close", "video_draw", "video_set_volume", "video_pause",
		"video_resume", "video_enable_loop", "video_seek_to", "video_get_duration",
		"video_get_position", "video_get_status", "video_get_format",

		// View functions
		"view_get_camera", "view_get_visible", "view_get_xport", "view_get_yport",
		"view_get_wport", "view_get_hport", "view_get_surface_id", "view_set_camera",
		"view_set_visible", "view_set_xport", "view_set_yport", "view_set_wport",
		"view_set_hport", "view_set_surface_id",

		// Variable functions (additional)
		"variable_instance_exists", "variable_instance_get", "variable_instance_get_names",
		"variable_instance_set", "variable_struct_exists", "variable_struct_get",
		"variable_struct_get_names", "variable_struct_set", "variable_struct_remove",

		// Window functions (additional)
		"window_enable_borderless_fullscreen", "window_get_borderless_fullscreen",
		"window_set_showborder", "window_get_showborder", "window_set_showicons",
		"window_get_showicons", "window_set_sizeable", "window_get_sizeable",
		"window_set_topmost", "window_get_topmost",

		// Miscellaneous missing functions
		"application_get_position", "application_surface_draw_enable", "application_surface_enable",
		"application_surface_is_enabled", "asset_has_tags", "asset_get_tags", "asset_add_tags",
		"asset_remove_tags", "asset_clear_tags", "code_is_compiled", "event_inherited",
		"gc_collect", "gc_enable", "gc_get_stats", "gc_is_enabled", "gc_target_frame_time",
		"room_tile_add", "room_tile_add_ext", "room_tile_clear", "tag_get_asset_ids",
		"tag_get_assets", "undefined",

		// Utility functions often missed
		"approach", "wrap", "typeof", "is_array", "is_bool", "is_infinity", "is_int32",
		"is_int64", "is_method", "is_nan", "is_numeric", "is_ptr", "is_real", "is_string",
		"is_struct", "is_undefined", "is_vec3", "is_vec4", "method", "method_get_index",
		"method_get_self", "ptr", "ref_create", "self_destroy", "weak_ref_alive",
		"weak_ref_create",

		// Array functions
		"array_create", "array_copy", "array_equals", "array_get", "array_height_2d",
		"array_insert", "array_length", "array_length_1d", "array_length_2d", "array_pop",
		"array_push", "array_resize", "array_reverse", "array_set", "array_shift",
		"array_shuffle", "array_sort", "array_unshift", "array_width_2d",

		// Struct functions
		"struct_exists", "struct_get", "struct_get_from_hash", "struct_get_names",
		"struct_has", "struct_remove", "struct_remove_from_hash", "struct_set",
		"struct_set_from_hash", "struct_names_count",

		// Additional debugging functions
		"debug_get_callstack", "show_debug_message", "show_debug_overlay",

		// Additional Asset functions
		"asset_get_index", "asset_get_type",

		// Font functions
		"font_exists", "font_get_name", "font_get_fontname", "font_get_bold", "font_get_italic",
		"font_get_first", "font_get_last", "font_get_size", "font_replace", "font_replace_sprite",
		"font_replace_sprite_ext", "font_delete", "font_duplicate", "font_add", "font_add_sprite",
		"font_add_sprite_ext", "font_cache_glyph", "font_uncache_glyph", "font_enable_aa",
		"font_disable_aa", "font_get_info", "font_set_cache_size",

		// Scheduler functions
		"scheduler_resolution_get", "scheduler_resolution_set",

		// Call functions
		"call_cancel", "call_later",

		// Rollback functions
		"rollback_define_input", "rollback_define_player", "rollback_get_info", "rollback_get_frame_count",
		"rollback_get_random_seed", "rollback_get_random_value", "rollback_join_game", "rollback_leave_game",
		"rollback_create_game", "rollback_use_random_input", "rollback_sync_on_frame", "rollback_get_current_frame",
		"rollback_apply_state", "rollback_save_state", "rollback_load_state", "rollback_use_manual_prediction",
		"rollback_use_player_prefs", "rollback_chat_send", "rollback_chat_enable",

		// Sequence functions
		"sequence_create", "sequence_destroy", "sequence_exists", "sequence_get", "sequence_set",
		"sequence_get_objects", "sequence_instance_override_object", "sequence_keyframe_new", "sequence_keyframedata_new",
		"sequence_keyframe_delete", "sequence_track_new", "sequence_track_delete", "sequence_get_track",
		"seqdir_right", "seqdir_left", "seqinterp_linear", "seqinterp_smooth", "seqinterp_bezier",

		// Skeleton animation functions
		"skeleton_animation_get", "skeleton_animation_set", "skeleton_animation_mix", "skeleton_animation_get_current",
		"skeleton_animation_get_ext", "skeleton_animation_set_ext", "skeleton_bone_data_get", "skeleton_bone_data_set",
		"skeleton_bone_state_get", "skeleton_bone_state_set", "skeleton_collision_draw_set", "skeleton_get_minmax",
		"skeleton_get_num_bounds", "skeleton_get_bounds", "skeleton_skin_get", "skeleton_skin_set",
		"skeleton_attachment_get", "skeleton_attachment_set", "skeleton_attachment_create", "skeleton_slot_data",

		// ZIP functions
		"zip_create", "zip_open", "zip_close", "zip_add_file", "zip_add_folder", "zip_extract_file",
		"zip_extract_all", "zip_get_num_files", "zip_get_name", "zip_file_exists", "zip_file_delete",

		// Camera functions
		"camera_create", "camera_create_view", "camera_destroy", "camera_apply",
		"camera_get_active", "camera_get_default", "camera_set_default",
		"camera_get_view_mat", "camera_get_proj_mat", "camera_get_update_script",
		"camera_get_begin_script", "camera_get_end_script", "camera_set_view_mat",
		"camera_set_proj_mat", "camera_set_update_script", "camera_set_begin_script",
		"camera_set_end_script", "camera_set_view_pos", "camera_set_view_size",
		"camera_set_view_speed", "camera_set_view_border", "camera_set_view_angle", "camera_set_view_target",
		"camera_get_view_x", "camera_get_view_y", "camera_get_view_width",
		"camera_get_view_height", "camera_get_view_speed_x", "camera_get_view_speed_y",
		"camera_get_view_border_x", "camera_get_view_border_y", "camera_get_view_angle", "camera_copy_transforms",

		// GPU functions
		"gpu_set_blendenable", "gpu_get_blendenable", "gpu_set_blendmode", "gpu_set_blendmode_ext",
		"gpu_set_blendmode_ext_sepalpha", "gpu_set_colorwriteenable", "gpu_get_colorwriteenable",
		"gpu_set_colourwriteenable", "gpu_get_colourwriteenable", "gpu_set_alphatestenable",
		"gpu_get_alphatestenable", "gpu_set_alphatestref", "gpu_get_alphatestref", "gpu_set_alphatestfunc",
		"gpu_get_alphatestfunc", "gpu_set_cullmode", "gpu_get_cullmode", "gpu_set_ztestenable",
		"gpu_get_ztestenable", "gpu_set_zfunc", "gpu_get_zfunc", "gpu_set_zwriteenable",
		"gpu_get_zwriteenable", "gpu_set_lightingenable", "gpu_get_lightingenable", "gpu_set_fog",
		"gpu_get_fog", "gpu_set_state", "gpu_get_state", "gpu_push_state", "gpu_pop_state",
		"gpu_set_tex_filter", "gpu_get_tex_filter", "gpu_set_tex_filter_ext", "gpu_get_tex_filter_ext",
		"gpu_set_tex_repeat", "gpu_get_tex_repeat", "gpu_set_tex_repeat_ext", "gpu_get_tex_repeat_ext",
		"gpu_set_tex_mip_filter", "gpu_get_tex_mip_filter", "gpu_set_tex_mip_bias", "gpu_get_tex_mip_bias",
		"gpu_set_tex_min_mip", "gpu_get_tex_min_mip", "gpu_set_tex_max_mip", "gpu_get_tex_max_mip",
		"gpu_set_tex_max_aniso", "gpu_get_tex_max_aniso", "gpu_set_tex_mip_enable", "gpu_get_tex_mip_enable",

		// Animation curve functions
		"animcurve_exists", "animcurve_get", "animcurve_get_channel", "animcurve_channel_evaluate",
		"animcurve_channel_new", "animcurve_channel_delete", "animcurve_point_new", "animcurve_point_delete",
		"animcurve_point_set", "animcurve_point_get", "animcurve_channel_get_point", "animcurve_channel_set_point",
		"animcurve_channel_get_num_points", "animcurve_get_channel_index", "animcurve_get_num_channels",

		// Audio functions (comprehensive)
		"audio_exists", "audio_get_name", "audio_play_sound", "audio_play_sound_on", "audio_play_sound_at",
		"audio_stop_sound", "audio_stop_all", "audio_pause_sound", "audio_pause_all", "audio_resume_sound",
		"audio_resume_all", "audio_sound_is_playable", "audio_sound_length", "audio_get_type", "audio_falloff_set_model",
		"audio_play_music", "audio_stop_music", "audio_pause_music", "audio_resume_music", "audio_music_is_playing",
		"audio_sound_gain", "audio_sound_pitch", "audio_get_listener_mask", "audio_set_listener_mask",
		"audio_get_listener_count", "audio_get_listener_info", "audio_set_listener_position",
		"audio_set_listener_velocity", "audio_set_listener_orientation", "audio_listener_set_position",
		"audio_listener_set_velocity", "audio_listener_set_orientation", "audio_listener_get_data",
		"audio_set_master_gain", "audio_get_master_gain", "audio_sound_set_track_position",
		"audio_sound_get_track_position", "audio_emitter_create", "audio_emitter_free", "audio_emitter_exists",
		"audio_emitter_position", "audio_emitter_create_stream", "audio_emitter_destroy_stream",
		"audio_emitter_velocity", "audio_emitter_falloff", "audio_emitter_gain", "audio_emitter_pitch",
		"audio_emitter_set_listener_mask", "audio_emitter_get_listener_mask", "audio_emitter_bus",
		"audio_bus_create", "audio_bus_main", "audio_sound_loop", "audio_sound_loop_start", "audio_sound_loop_end",
		"audio_create_stream", "audio_destroy_stream", "audio_create_sync_group", "audio_destroy_sync_group",
		"audio_play_in_sync_group", "audio_start_sync_group", "audio_stop_sync_group", "audio_pause_sync_group",
		"audio_resume_sync_group", "audio_sync_group_get_track_pos", "audio_sync_group_debug",
		"audio_sync_group_is_playing", "audio_debug", "audio_group_load", "audio_group_unload",
		"audio_group_is_loaded", "audio_group_name", "audio_group_set_gain", "audio_create_buffer_sound",
		"audio_free_buffer_sound", "audio_create_play_queue", "audio_free_play_queue", "audio_queue_sound",
		"audio_get_recorder_count", "audio_get_recorder_info", "audio_start_recording", "audio_stop_recording",
		"audio_sound_get_gain", "audio_sound_get_pitch", "audio_sound_get_listener_mask",
		"audio_sound_set_listener_mask", "audio_sound_get_bus", "audio_sound_set_bus", "audio_listener_orientation",

		// Extra
		"exception_unhandled_handler", "extension_get_option_value", "extension_exists", "extension_get_options", "extension_get_version", "extension_get_name", "variable_clone", "variable_get_hash", "nameof",
	}

	for _, fn := range functions {
		s.functions[fn] = CompletionItem{
			Label:         fn,
			Kind:          CompletionItemKindFunction,
			Detail:        "GML built-in function",
			Documentation: fmt.Sprintf("Built-in GML function: %s", fn),
		}
	}

	// Constants
	constants := []string{
		// Colors
		"c_aqua", "c_black", "c_blue", "c_dkgray", "c_fuchsia", "c_gray", "c_white", "c_green", "c_lime", "c_ltgray", "c_maroon", "c_navy", "c_olive", "c_orange",
		"c_purple", "c_red", "c_silver", "c_teal", "c_yellow",

		// Gamespeed

		"gamespeed_fps", "gamespeed_microseconds",

		// Blend constants
		"bm_add", "bm_dest_alpha", "bm_dest_color", "bm_dest_colour", "bm_inv_dest_colour", "bm_inv_dest_color", "bm_inv_dest_alpha", "bm_inv_src_color", "bm_inv_src_colour",
		"bm_inv_src_alpha", "bm_max", "bm_normal", "bm_one", "bm_src_alpha", "bm_src_alpha_sat", "bm_src_color", "bm_src_colour", "bm_subtract", "bm_zero",

		// Buffer constants
		"buffer_fixed", "buffer_grow", "buffer_wrap", "buffer_fast", "buffer_vbuffer",
		"buffer_u8", "buffer_s8", "buffer_u16", "buffer_s16", "buffer_u32", "buffer_s32", "buffer_u64", "buffer_f16", "buffer_f32", "buffer_f64", "buffer_bool", "buffer_string", "buffer_text",
		"buffer_network", "buffer_seek_start", "buffer_seek_relative", "buffer_seek_end",
		"buffer_generalerror", "buffer_outofspace", "buffer_outofbounds", "buffer_invalidtype",

		// Flex panel

		"flexpanel_align", "flexpanel_direction", "flexpanel_display", "flexpanel_edge", "flexpanel_flex_direction", "flexpanel_gutter", "flexpanel_justify", "flexpanel_position_type", "flexpanel_unit", "flexpanel_wrap",

		// Math constants
		"pi", "infinity", "NaN",

		// Special identifiers/constants
		"self", "other", "noone", "all", "undefined", "pointer_null", "pointer_invalid",

		// System constants
		"debug_mode", "timezone_local", "timezone_utc",

		// Kbv
		"kbv_autocapitalize_characters", "kbv_autocapitalize_none", "kbv_autocapitalize_sentences", "kbv_autocapitalize_words", "kbv_returnkey_continue", "kbv_returnkey_default", "kbv_returnkey_done", "kbv_returnkey_emergency", "kbv_returnkey_go", "kbv_returnkey_google", "kbv_returnkey_join", "kbv_returnkey_next", "kbv_returnkey_route", "kbv_returnkey_search", "kbv_returnkey_yahoo", "kbv_type_ascii", "kbv_type_default", "kbv_type_email", "kbv_type_numbers", "kbv_type_phone", "kbv_type_phone_name", "kbv_type_url",

		// Stencilop

		"stencilop_decr", "stencilop_decr_wrap", "stencilop_incr", "stencilop_incr_wrap", "stencilop_invert", "stencilop_keep", "stencilop_replace", "stencilop_zero",

		// Path action constants
		"path_action_stop", "path_action_restart", "path_action_continue", "path_action_reverse",

		// Event constants
		"ev_create", "ev_destroy", "ev_step", "ev_alarm", "ev_keyboard", "ev_mouse", "ev_collision", "ev_other", "ev_draw", "ev_draw_begin", "ev_draw_end", "ev_draw_pre", "ev_draw_post",
		"ev_keypress", "ev_keyrelease", "ev_trigger", "ev_left_button", "ev_right_button", "ev_middle_button", "ev_no_button", "ev_left_press", "ev_right_press", "ev_middle_press",
		"ev_left_release", "ev_right_release", "ev_middle_release", "ev_mouse_enter", "ev_mouse_leave", "ev_mouse_wheel_up", "ev_mouse_wheel_down", "ev_global_left_button",
		"ev_global_right_button", "ev_global_middle_button", "ev_global_left_press", "ev_global_right_press", "ev_global_middle_press", "ev_global_left_release",
		"ev_global_right_release", "ev_global_middle_release",
		"ev_outside", "ev_boundary", "ev_game_start", "ev_game_end", "ev_room_start", "ev_room_end", "ev_no_more_lives", "ev_animation_end", "ev_end_of_path",
		"ev_no_more_health", "ev_close_button", "ev_user0", "ev_user1", "ev_user2", "ev_user3", "ev_user4", "ev_user5", "ev_user6", "ev_user7", "ev_user8", "ev_user9", "ev_user10", "ev_user11",
		"ev_user12", "ev_user13", "ev_user14", "ev_user15", "ev_step_normal", "ev_step_begin", "ev_step_end", "ev_gui", "ev_gui_begin", "ev_gui_end",

		// Virtual keys
		"vk_nokey", "vk_anykey", "vk_enter", "vk_return", "vk_shift", "vk_control", "vk_alt", "vk_escape", "vk_space", "vk_backspace", "vk_tab", "vk_pause", "vk_printscreen", "vk_left",
		"vk_right", "vk_up", "vk_down", "vk_home", "vk_end", "vk_delete", "vk_insert", "vk_pageup", "vk_pagedown", "vk_f1", "vk_f2", "vk_f3", "vk_f4", "vk_f5", "vk_f6", "vk_f7", "vk_f8", "vk_f9", "vk_f10",
		"vk_f11", "vk_f12", "vk_numpad0", "vk_numpad1", "vk_numpad2", "vk_numpad3", "vk_numpad4", "vk_numpad5", "vk_numpad6", "vk_numpad7", "vk_numpad8", "vk_numpad9", "vk_divide", "vk_multiply",
		"vk_subtract", "vk_add", "vk_decimal", "vk_lshift", "vk_lcontrol", "vk_lalt", "vk_rshift", "vk_rcontrol", "vk_ralt",

		// Mouse buttons
		"mb_any", "mb_none", "mb_left", "mb_right", "mb_middle", "mb_side1", "mb_side2",

		// Alignment constants
		"fa_left", "fa_center", "fa_right", "fa_top", "fa_middle", "fa_bottom",

		// Primitive types
		"pr_pointlist", "pr_linelist", "pr_linestrip", "pr_trianglelist", "pr_trianglestrip", "pr_trianglefan",

		// Audio falloff types
		"audio_falloff_none", "audio_falloff_inverse_distance", "audio_falloff_inverse_distance_clamped", "audio_falloff_linear_distance",
		"audio_falloff_linear_distance_clamped", "audio_falloff_exponent_distance", "audio_falloff_exponent_distance_clamped",

		// Audio types
		"audio_old_system", "audio_new_system", "audio_mono", "audio_stereo", "audio_3d",

		// Vertex buffer modes
		"vbm_fast", "vbm_compatible", "vbm_most_compatible",

		// Cursor types
		"cr_default", "cr_none", "cr_arrow", "cr_cross", "cr_beam", "cr_size_nesw", "cr_size_ns", "cr_size_nwse", "cr_size_we", "cr_uparrow", "cr_hourglass", "cr_drag",
		"cr_appstart", "cr_handpoint", "cr_size_all",

		// Asset types
		"asset_object", "asset_unknown", "asset_sprite", "asset_sound", "asset_room", "asset_background", "asset_path", "asset_script", "asset_font", "asset_timeline",

		// File attributes
		"fa_readonly", "fa_hidden", "fa_sysfile", "fa_volumeid", "fa_directory", "fa_archive",

		// Data structure types
		"ds_type_map", "ds_type_list", "ds_type_stack", "ds_type_queue", "ds_type_grid", "ds_type_priority",

		// Effect types
		"ef_explosion", "ef_ring", "ef_ellipse", "ef_firework", "ef_smoke", "ef_smokeup", "ef_star", "ef_spark", "ef_flare", "ef_cloud", "ef_rain", "ef_snow",

		// Particle types
		"pt_shape_pixel", "pt_shape_disk", "pt_shape_square", "pt_shape_line", "pt_shape_star", "pt_shape_circle", "pt_shape_ring", "pt_shape_sphere", "pt_shape_flare", "pt_shape_spark",
		"pt_shape_explosion", "pt_shape_cloud", "pt_shape_smoke", "pt_shape_snow",

		// Particle system types
		"ps_distr_linear", "ps_distr_gaussian", "ps_distr_invgaussian", "ps_shape_rectangle", "ps_shape_ellipse", "ps_shape_diamond", "ps_shape_line",

		// Type constants
		"ty_real", "ty_string", "dll_cdecl", "dll_stdcall",

		// Matrix types
		"matrix_view", "matrix_projection", "matrix_world",

		// OS types
		"os_win32", "os_windows", "os_macosx", "os_psp", "os_ios", "os_android", "os_symbian", "os_linux", "os_unknown", "os_winphone", "os_tizen", "os_win8native", "os_wiiu", "os_3ds",
		"os_psvita", "os_bb10", "os_ps4", "os_xboxone", "os_ps3", "os_xbox360", "os_uwp",

		// Browser types
		"browser_not_a_browser", "browser_unknown", "browser_ie", "browser_firefox", "browser_chrome", "browser_safari", "browser_safari_mobile", "browser_opera", "browser_tizen",
		"browser_windows_store", "browser_ie_mobile",

		// Device types
		"device_ios_unknown", "device_ios_iphone", "device_ios_iphone_retina", "device_ios_ipad", "device_ios_ipad_retina", "device_ios_iphone5", "device_ios_iphone6",
		"device_ios_iphone6plus", "device_ios_iphone6s", "device_ios_iphone6splus",
		"device_emulator", "device_tablet",

		// Display orientations
		"display_landscape", "display_landscape_flipped", "display_portrait", "display_portrait_flipped",

		// Leaderboard types
		"leaderboard_type_number", "leaderboard_type_time_mins_secs",

		// IAP event types
		"iap_ev_storeload", "iap_ev_product", "iap_ev_purchase", "iap_ev_consume", "iap_ev_restore", "iap_storeload_ok", "iap_storeload_failed",

		// IAP status types
		"iap_status_uninitialised", "iap_status_unavailable", "iap_status_loading", "iap_status_available", "iap_status_processing", "iap_status_restoring",

		// IAP purchase status types
		"iap_failed", "iap_unavailable", "iap_available", "iap_purchased", "iap_canceled", "iap_refunded",

		// Facebook login types
		"fb_login_default", "fb_login_fallback_to_webview", "fb_login_no_fallback_to_webview", "fb_login_forcing_webview", "fb_login_use_system_account",
		"fb_login_forcing_safari",

		// Physics joint constants
		"phy_joint_anchor_1_x", "phy_joint_anchor_1_y", "phy_joint_anchor_2_x", "phy_joint_anchor_2_y", "phy_joint_reaction_force_x",
		"phy_joint_reaction_force_y", "phy_joint_reaction_torque", "phy_joint_motor_speed", "phy_joint_angle", "phy_joint_motor_torque", "phy_joint_max_motor_torque",
		"phy_joint_translation", "phy_joint_speed", "phy_joint_motor_force", "phy_joint_max_motor_force", "phy_joint_length_1", "phy_joint_length_2",
		"phy_joint_damping_ratio", "phy_joint_frequency", "phy_joint_lower_angle_limit", "phy_joint_upper_angle_limit", "phy_joint_angle_limits",
		"phy_joint_max_length", "phy_joint_max_torque", "phy_joint_max_force",

		// Physics debug render flags
		"phy_debug_render_aabb", "phy_debug_render_collision_pairs", "phy_debug_render_coms", "phy_debug_render_core_shapes", "phy_debug_render_joints",
		"phy_debug_render_obb", "phy_debug_render_shapes",

		// Physics particle flags
		"phy_particle_flag_water", "phy_particle_flag_zombie", "phy_particle_flag_wall", "phy_particle_flag_spring", "phy_particle_flag_elastic",
		"phy_particle_flag_viscous", "phy_particle_flag_powder", "phy_particle_flag_tensile", "phy_particle_flag_colourmixing", "phy_particle_flag_colormixing",
		"phy_particle_group_flag_solid", "phy_particle_group_flag_rigid",

		// Physics particle data flags
		"phy_particle_data_flag_typeflags", "phy_particle_data_flag_position", "phy_particle_data_flag_velocity", "phy_particle_data_flag_colour",
		"phy_particle_data_flag_color", "phy_particle_data_flag_category",

		// Achievement constants
		"achievement_our_info", "achievement_friends_info", "achievement_leaderboard_info", "achievement_achievement_info", "achievement_filter_all_players",
		"achievement_filter_friends_only", "achievement_filter_favorites_only",
		"achievement_type_achievement_challenge", "achievement_type_score_challenge", "achievement_pic_loaded",
		"achievement_show_ui", "achievement_show_profile", "achievement_show_leaderboard", "achievement_show_achievement", "achievement_show_bank",
		"achievement_show_friend_picker", "achievement_show_purchase_prompt",

		// Network socket types
		"network_socket_tcp", "network_socket_udp", "network_socket_bluetooth", "network_type_connect", "network_type_disconnect", "network_type_data",
		"network_type_non_blocking_connect",

		// Network config types
		"network_config_connect_timeout", "network_config_use_non_blocking_socket", "network_config_enable_reliable_udp", "network_config_disable_reliable_udp",

		// Gamepad constants
		"gp_face1", "gp_face2", "gp_face3", "gp_face4", "gp_shoulderl", "gp_shoulderr", "gp_shoulderlb", "gp_shoulderrb", "gp_select", "gp_start", "gp_stickl", "gp_stickr", "gp_padu", "gp_padd",
		"gp_padl", "gp_padr", "gp_axislh", "gp_axislv", "gp_axisrh", "gp_axisrv",
		"gp_axis_acceleration_z", "gp_axis_angular_velocity_z", "gp_axis_acceleration_y", "gp_axis_acceleration_x", "gp_axis_angular_velocity_x", "gp_axis_angular_velocity_y", "gp_axis_orientation_w", "gp_axis_orientation_x", "gp_axis_orientation_y", "gp_axis_orientation_z",

		// Video constants
		"video_format_rgba", "video_format_yuv", "video_status_closed", "video_status_paused", "video_status_playing", "video_status_preparing",

		// Vertex usage types
		"vertex_usage_position", "vertex_usage_colour", "vertex_usage_color", "vertex_usage_normal", "vertex_usage_textcoord", "vertex_usage_blendweight",
		"vertex_usage_blendindices", "vertex_usage_psize", "vertex_usage_tangent", "vertex_usage_binormal", "vertex_usage_fog", "vertex_usage_depth", "vertex_usage_sample",

		// Vertex type constants
		"vertex_type_float1", "vertex_type_float2", "vertex_type_float3", "vertex_type_float4", "vertex_type_colour", "vertex_type_color", "vertex_type_ubyte4",

		// Steam constants
		"ov_friends", "ov_community", "ov_players", "ov_settings", "ov_gamegroup", "ov_achievements", "lb_sort_none", "lb_sort_ascending", "lb_sort_descending",
		"lb_disp_none", "lb_disp_numeric", "lb_disp_time_sec", "lb_disp_time_ms",
		"ugc_result_success", "ugc_filetype_community", "ugc_filetype_microtrans", "ugc_visibility_public", "ugc_visibility_friends_only", "ugc_visibility_private",

		"seqplay_oneshot", "seqplay_loop", "seqplay_pingpong",
		"seqtracktype_graphic", "seqtracktype_audio", "seqtracktype_real", "seqtracktype_bool",
		"seqtracktype_string", "seqtracktype_color", "seqtracktype_colour",

		// Rollback constants
		"rollback_chat_channel_all", "rollback_chat_channel_host",

		// GPU constants
		"gpu_ps_4", "gpu_vs_4", "gpu_vs_5", "gpu_ps_5",
		"tf_point", "tf_linear", "tf_anisotropic",
		"mip_off", "mip_on", "mip_markedonly",
		"cull_noculling", "cull_clockwise", "cull_counterclockwise",
		"cmpfunc_never", "cmpfunc_less", "cmpfunc_equal", "cmpfunc_lessequal",
		"cmpfunc_greater", "cmpfunc_notequal", "cmpfunc_greaterequal", "cmpfunc_always",

		// Audio constants
		"audio_sound_effect", "audio_voice_effect", "audio_stream_effect",

		// Path constants
		"path_action_stop", "path_action_restart", "path_action_continue", "path_action_reverse",

		// Tile constants
		"tile_rotate", "tile_flip", "tile_mirror", "tile_index_mask",

		// Additional sequence constants
		"seqtextkey_bottom", "seqtextkey_top", "seqtextkey_center", "seqtextkey_justify",
		"seqtextkey_middle", "seqtextkey_right", "seqtextkey_left",

		// Time source constants
		"time_source_units_seconds", "time_source_units_frames",
		"time_source_state_initial", "time_source_state_active", "time_source_state_paused", "time_source_state_stopped",
		"time_source_expire_nearest", "time_source_expire_after",

		// Surface constants
		"surface_r8unorm", "surface_rg8unorm", "surface_rgba8unorm", "surface_rgba4unorm",
		"surface_r16float", "surface_rg16float", "surface_rgba16float", "surface_r32float",
		"surface_rg32float", "surface_rgba32float",

		// Additional layer constants
		"layerelementtype_undefined", "layerelementtype_background", "layerelementtype_instance",
		"layerelementtype_oldtilemap", "layerelementtype_sprite", "layerelementtype_tilemap",
		"layerelementtype_particlesystem", "layerelementtype_tile", "layerelementtype_sequence",

		// Additional sprite constants
		"spritespeed_framespersecond", "spritespeed_framespergameframe",

		// Aditional animation curves
		"animcurvetype_bezier", "animcurvetype_linear", "animcurve_catmullrom",

		// Text align
		"textalign_bottom", "textalign_top", "textalign_center", "textalign_middle", "textalign_right", "textalign_left", "textalign_justify",

		// Additional collision constants
		"bboxmode_automatic", "bboxmode_fullimage", "bboxmode_manual", "bboxkind_diamond", "bboxkind_ellipse", "bboxkind_precise", "bboxkind_rectangular", "bboxkind_spine", "move_and_collide",
	}

	for _, constant := range constants {
		s.constants[constant] = CompletionItem{
			Label:  constant,
			Kind:   CompletionItemKindConstant,
			Detail: "GML constant",
		}
	}

	// Built-in variables (including read-only variables and system variables)
	variables := []string{
		// Instance variables
		"x", "y", "speed", "direction", "hspeed", "vspeed",
		"sprite_index", "image_index", "image_speed", "image_xscale", "image_yscale",
		"image_angle", "image_alpha", "image_blend",
		"visible", "solid", "persistent", "depth", "alarm",
		"id", "instance_id", "object_index", "instance_count", "mask_index", "managed",
		"xprevious", "yprevious", "xstart", "ystart", "layer",
		"friction", "gravity", "gravity_direction", "in_collision_tree", "wallpaper_config",
		"browser_width", "browser_height", "on_ui_layer", "player_avatar_url", "player_user_id",
		"fps", "fps_real", "current_fps", "room_persistent", "in_sequence", "cache_directory",

		// Path variables
		"path_index", "path_position", "path_positionprevious", "path_speed", "path_scale", "path_orientation", "path_endaction",

		// Timeline variables
		"timeline_index", "timeline_position", "timeline_speed", "timeline_running", "timeline_loop",

		// Room variables
		"room", "room_speed", "room_width", "room_height", "room_first", "room_last", "room_caption", "room_persistent",

		// Input variables
		"mouse_x", "mouse_y", "mouse_button", "mouse_lastbutton",
		"keyboard_key", "keyboard_lastkey", "keyboard_lastchar", "keyboard_string",

		// Device mouse variables
		"device_mouse_x", "device_mouse_y", "device_mouse_raw_x", "device_mouse_raw_y", "device_mouse_x_to_gui", "device_mouse_y_to_gui",

		// Event variables
		"event_type", "event_number", "event_object", "event_action",

		// Sprite properties (read-only)
		"sprite_width", "sprite_height", "sprite_xoffset", "sprite_yoffset", "image_number",
		"bbox_left", "bbox_right", "bbox_top", "bbox_bottom",

		// Background variables
		"background_colour", "background_color", "background_showcolour", "background_showcolor",

		// View variables
		"view_enabled", "view_current", "view_visible", "view_xport", "view_yport", "view_wport", "view_hport", "view_surface_id",

		// Game variables
		"score", "health", "lives",
		"game_id", "game_display_name", "game_project_name", "game_save_id",

		// Directory variables (read-only)
		"working_directory", "temp_directory", "program_directory",

		// System variables (read-only)
		"os_type", "os_device", "os_browser", "os_version",
		"display_aa", "webgl_enabled",

		// Time variables (read-only)
		"current_time", "current_year", "current_month", "current_day", "current_weekday",
		"current_hour", "current_minute", "current_second", "delta_time",

		// Physics variables
		"phy_rotation", "phy_position_x", "phy_position_y", "phy_angular_velocity", "phy_linear_velocity_x", "phy_linear_velocity_y",
		"phy_speed_x", "phy_speed_y", "phy_speed", "phy_angular_damping", "phy_linear_damping", "phy_bullet",
		"phy_fixed_rotation", "phy_active", "phy_mass", "phy_inertia", "phy_com_x", "phy_com_y", "phy_dynamic",
		"phy_kinematic", "phy_sleeping", "phy_collision_points", "phy_collision_x", "phy_collision_y", "phy_col_normal_x", "phy_col_normal_y",
		"phy_position_xprevious", "phy_position_yprevious",

		// Script/function variables
		"argument", "argument_count", "argument_relative", "argument0", "argument1", "argument2", "argument3", "argument4", "argument5",
		"argument6", "argument7", "argument8", "argument9", "argument10", "argument11",
		"argument12", "argument13", "argument14", "argument15",

		// Event data
		"event_data", "async_load",

		// Surfaces
		"application_surface",

		// Misc variables
		"cursor_sprite",

		// System read-only variables
		"GM_build_date", "GM_version", "GM_project_filename", "GM_is_sandboxed", "GM_build_type", "GM_runtime_version",

		// Sequence variables
		"sequence", "sequence_instance",

		// Rollback variables
		"rollback_event_id", "rollback_connected_players", "rollback_local_player_id",

		// GPU state variables
		"gpu_state", "gpu_cullmode_none", "gpu_cullmode_clockwise", "gpu_cullmode_counterclockwise",
		"gpu_cmpfunc_never", "gpu_cmpfunc_less", "gpu_cmpfunc_equal", "gpu_cmpfunc_lessequal",
		"gpu_cmpfunc_greater", "gpu_cmpfunc_notequal", "gpu_cmpfunc_greaterequal", "gpu_cmpfunc_always",

		// Audio variables
		"audio_sound_volume", "audio_sound_pitch", "audio_listener_count", "audio_channel_num",

		// Animation curve variables
		"ac_linear", "ac_smooth", "ac_bezier",

		// Call later variables
		"call_later_handle",
	}

	for _, variable := range variables {
		s.variables[variable] = CompletionItem{
			Label:  variable,
			Kind:   CompletionItemKindVariable,
			Detail: "GML built-in variable",
		}
	}

	gmlFiles, err := s.scanDirectoryForGMLFiles(rootPath)
	if err != nil {
		log.Printf("Error scanning directory: %v", err)
		return
	}

	for _, file := range gmlFiles {
		s.parseGMLFile(file)
	}

	assetCompletions := s.scanDirectoriesForAssets(rootPath)
	for label, item := range assetCompletions {
		s.keywords[label] = item
	}
}

func (s *GMLLanguageServer) handleMessage(msg *Message) *Message {
	switch msg.Method {
	case "initialize":
		return s.handleInitialize(msg)
	case "initialized":
		log.Println("Client is initialized.")
		return nil
	case "shutdown":
		return s.handleShutdown(msg)
	case "exit":
		s.handleExit()
		return nil
	case "textDocument/didOpen":
		s.handleDidOpen(msg)
		return nil
	case "textDocument/didChange":
		s.handleDidChange(msg)
		return nil
	case "textDocument/completion":
		return s.handleCompletion(msg)
	case "textDocument/hover":
		return s.handleHover(msg)
	case "textDocument/semanticTokens/full":
		return s.handleSemanticTokens(msg)
	default:
		log.Printf("Method not found: %s\n", msg.Method)
		return nil
	}
}

func (s *GMLLanguageServer) handleInitialize(msg *Message) *Message {
	var params InitializeParams
	paramsBytes, err := json.Marshal(msg.Params)
	if err != nil {
		log.Printf("Could not marshal initialize params: %v", err)
		return &Message{Jsonrpc: "2.0", ID: msg.ID, Error: &RPCError{Code: -32602, Message: "Invalid params"}}
	}
	if err := json.Unmarshal(paramsBytes, &params); err != nil {
		log.Printf("Could not unmarshal initialize params: %v", err)
		return &Message{Jsonrpc: "2.0", ID: msg.ID, Error: &RPCError{Code: -32602, Message: "Invalid params"}}
	}

	// Scan directory for GML files and initialize completions
	if params.RootPath != "" {
		s.initializeCompletions(params.RootPath)
	}

	capabilities := ServerCapabilities{
		TextDocumentSync: 1, // Full sync
		CompletionProvider: &CompletionOptions{
			ResolveProvider:   true,
			TriggerCharacters: []string{".", "_"},
		},
		HoverProvider:          true,
		DocumentSymbolProvider: true,
		SemanticTokensProvider: &SemanticTokensOptions{
			Legend: SemanticTokensLegend{
				TokenTypes:     s.tokenTypes,
				TokenModifiers: s.tokenModifiers,
			},
			Full:  true,
			Range: false,
		},
	}
	return &Message{
		Jsonrpc: "2.0",
		ID:      msg.ID,
		Result: map[string]interface{}{
			"capabilities": capabilities,
		},
	}
}

func (s *GMLLanguageServer) handleShutdown(msg *Message) *Message {
	log.Println("Shutdown request received.")
	s.shutdownReceived = true
	return &Message{
		Jsonrpc: "2.0",
		ID:      msg.ID,
		Result:  nil,
	}
}

func (s *GMLLanguageServer) handleExit() {
	log.Println("Exit notification received. Terminating.")
	if s.shutdownReceived {
		os.Exit(0)
	}
	os.Exit(1)
}

func (s *GMLLanguageServer) handleDidOpen(msg *Message) {
	var params DidOpenTextDocumentParams
	paramsBytes, err := json.Marshal(msg.Params)
	if err != nil {
		log.Printf("Could not marshal didOpen params: %v", err)
		return
	}
	if err := json.Unmarshal(paramsBytes, &params); err != nil {
		log.Printf("Could not unmarshal didOpen params: %v", err)
		return
	}

	s.documents[params.TextDocument.URI] = params.TextDocument.Text
	log.Printf("Opened document: %s", params.TextDocument.URI)
}

func (s *GMLLanguageServer) handleDidChange(msg *Message) {
	var params DidChangeTextDocumentParams
	paramsBytes, err := json.Marshal(msg.Params)
	if err != nil {
		log.Printf("Could not marshal didChange params: %v", err)
		return
	}
	if err := json.Unmarshal(paramsBytes, &params); err != nil {
		log.Printf("Could not unmarshal didChange params: %v", err)
		return
	}

	// For simplicity, we assume full document sync
	if len(params.ContentChanges) > 0 {
		s.documents[params.TextDocument.URI] = params.ContentChanges[0].Text
	}
	log.Printf("Changed document: %s", params.TextDocument.URI)
}

func (s *GMLLanguageServer) handleCompletion(msg *Message) *Message {
	var params CompletionParams
	paramsBytes, _ := json.Marshal(msg.Params)
	if err := json.Unmarshal(paramsBytes, &params); err != nil {
		return &Message{
			Jsonrpc: "2.0",
			ID:      msg.ID,
			Error: &RPCError{
				Code:    -32602,
				Message: "Invalid params for completion",
			},
		}
	}

	// Get document content
	content, exists := s.documents[params.TextDocument.URI]
	if !exists {
		content = ""
	}

	// Get the word being typed
	lines := strings.Split(content, "\n")
	if params.Position.Line >= len(lines) {
		return s.emptyCompletion(msg)
	}

	line := lines[params.Position.Line]
	if params.Position.Character > len(line) {
		return s.emptyCompletion(msg)
	}

	// Find the word boundary
	start := params.Position.Character
	for start > 0 && (unicode.IsLetter(rune(line[start-1])) || unicode.IsDigit(rune(line[start-1])) || line[start-1] == '_') {
		start--
	}

	prefix := line[start:params.Position.Character]

	// Generate completions
	completions := s.getCompletions(prefix)

	return &Message{
		Jsonrpc: "2.0",
		ID:      msg.ID,
		Result: CompletionList{
			IsIncomplete: false,
			Items:        completions,
		},
	}
}

func (s *GMLLanguageServer) getCompletions(prefix string) []CompletionItem {
	items := make([]CompletionItem, 0)
	prefix = strings.ToLower(prefix)

	// Add matching keywords
	for _, item := range s.keywords {
		if strings.HasPrefix(strings.ToLower(item.Label), prefix) {
			items = append(items, item)
		}
	}

	// Add matching functions
	for _, item := range s.functions {
		if strings.HasPrefix(strings.ToLower(item.Label), prefix) {
			items = append(items, item)
		}
	}

	// Add matching constants
	for _, item := range s.constants {
		if strings.HasPrefix(strings.ToLower(item.Label), prefix) {
			items = append(items, item)
		}
	}

	// Add matching variables
	for _, item := range s.variables {
		if strings.HasPrefix(strings.ToLower(item.Label), prefix) {
			items = append(items, item)
		}
	}

	// Sort by relevance (exact matches first, then alphabetical)
	sort.Slice(items, func(i, j int) bool {
		aExact := strings.ToLower(items[i].Label) == prefix
		bExact := strings.ToLower(items[j].Label) == prefix

		if aExact && !bExact {
			return true
		}
		if !aExact && bExact {
			return false
		}

		return items[i].Label < items[j].Label
	})

	// Limit results
	if len(items) > 50 {
		items = items[:50]
	}

	return items
}

func (s *GMLLanguageServer) handleHover(msg *Message) *Message {
	var params TextDocumentPositionParams
	paramsBytes, err := json.Marshal(msg.Params)
	if err != nil {
		return &Message{Jsonrpc: "2.0", ID: msg.ID, Result: nil}
	}
	if err := json.Unmarshal(paramsBytes, &params); err != nil {
		return &Message{Jsonrpc: "2.0", ID: msg.ID, Result: nil}
	}

	// Get document content and find word at position
	content, exists := s.documents[params.TextDocument.URI]
	if !exists {
		return &Message{Jsonrpc: "2.0", ID: msg.ID, Result: nil}
	}

	lines := strings.Split(content, "\n")
	if params.Position.Line >= len(lines) {
		return &Message{Jsonrpc: "2.0", ID: msg.ID, Result: nil}
	}

	line := lines[params.Position.Line]
	word := s.getWordAtPosition(line, params.Position.Character)

	if word == "" {
		return &Message{Jsonrpc: "2.0", ID: msg.ID, Result: nil}
	}

	// Look up documentation
	var documentation string

	if item, exists := s.functions[word]; exists {
		documentation = fmt.Sprintf("**%s** (function)\n\n%s", item.Label, item.Documentation)
	} else if item, exists := s.constants[word]; exists {
		documentation = fmt.Sprintf("**%s** (constant)\n\n%s", item.Label, item.Detail)
	} else if item, exists := s.variables[word]; exists {
		documentation = fmt.Sprintf("**%s** (variable)\n\n%s", item.Label, item.Detail)
	} else if item, exists := s.keywords[word]; exists {
		documentation = fmt.Sprintf("**%s** (keyword)\n\n%s", item.Label, item.Detail)
	}

	if documentation == "" {
		return &Message{Jsonrpc: "2.0", ID: msg.ID, Result: nil}
	}

	return &Message{
		Jsonrpc: "2.0",
		ID:      msg.ID,
		Result: Hover{
			Contents: documentation,
		},
	}
}

func (s *GMLLanguageServer) getWordAtPosition(line string, character int) string {
	if character >= len(line) {
		// Handle cursor at the end of the line
		if character > 0 && (unicode.IsLetter(rune(line[character-1])) || unicode.IsDigit(rune(line[character-1])) || line[character-1] == '_') {
			character-- // Step back to be inside the word
		} else {
			return ""
		}
	}

	// Find word boundaries
	start := character
	end := character

	// Move start backward
	for start > 0 && (unicode.IsLetter(rune(line[start-1])) || unicode.IsDigit(rune(line[start-1])) || line[start-1] == '_') {
		start--
	}

	// Move end forward
	for end < len(line) && (unicode.IsLetter(rune(line[end])) || unicode.IsDigit(rune(line[end])) || line[end] == '_') {
		end++
	}

	if start == end {
		return ""
	}

	return line[start:end]
}

func (s *GMLLanguageServer) handleSemanticTokens(msg *Message) *Message {
	var params SemanticTokensParams
	paramsBytes, err := json.Marshal(msg.Params)
	if err != nil {
		return &Message{Jsonrpc: "2.0", ID: msg.ID, Result: nil}
	}
	if err := json.Unmarshal(paramsBytes, &params); err != nil {
		return &Message{
			Jsonrpc: "2.0",
			ID:      msg.ID,
			Error: &RPCError{
				Code:    -32602,
				Message: "Invalid params for semantic tokens",
			},
		}
	}

	// Get document content
	content, exists := s.documents[params.TextDocument.URI]
	if !exists {
		content = ""
	}

	// Tokenize the content
	tokens := s.tokenizeContent(content)

	// Convert to LSP semantic tokens format
	data := s.tokensToLSPData(tokens)

	return &Message{
		Jsonrpc: "2.0",
		ID:      msg.ID,
		Result: SemanticTokens{
			Data: data,
		},
	}
}

func (s *GMLLanguageServer) tokenizeContent(content string) []GMLToken {
	var tokens []GMLToken
	lines := strings.Split(content, "\n")

	// Create a combined regex pattern for keywords
	keywordsPattern := strings.Join(s.getKeys(s.keywords), "|")
	// Create a combined regex pattern for functions
	functionsPattern := strings.Join(s.getKeys(s.functions), "|")
	// Create a combined regex pattern for variables
	variablesPattern := strings.Join(s.getKeys(s.variables), "|")
	// Create a combined regex pattern for constants
	constantsPattern := strings.Join(s.getKeys(s.constants), "|")

	// Regex patterns for different token types
	patterns := map[int]*regexp.Regexp{
		SemanticTokenTypeKeyword:  regexp.MustCompile(`\b(` + keywordsPattern + `)\b`),
		SemanticTokenTypeFunction: regexp.MustCompile(`\b(` + functionsPattern + `)\b`),
		SemanticTokenTypeString:   regexp.MustCompile(`"([^"\\]|\\.)*"`),
		SemanticTokenTypeNumber:   regexp.MustCompile(`\b\d+\.?\d*\b`),
		SemanticTokenTypeComment:  regexp.MustCompile(`//.*$|/\*[\s\S]*?\*/`),
		SemanticTokenTypeOperator: regexp.MustCompile(`[+\-*/%=<>!&|^~]+`),
		SemanticTokenTypeVariable: regexp.MustCompile(`\b(` + variablesPattern + `)\b`),
		SemanticTokenTypeProperty: regexp.MustCompile(`\b(` + constantsPattern + `)\b`),
		SemanticTokenTypeMacro:    regexp.MustCompile(`#\s*(macro|region|endregion)\b`),
	}

	// Add user-defined variable detection
	userVariablePattern := regexp.MustCompile(`\b[a-zA-Z_][a-zA-Z0-9_]*\b`)

	for lineNum, line := range lines {
		// Skip empty lines
		if strings.TrimSpace(line) == "" {
			continue
		}
		// Track positions that have been tokenized to avoid overlaps
		covered := make([]bool, len(line))

		// Process comments first (they have highest precedence)
		if matches := patterns[SemanticTokenTypeComment].FindAllStringIndex(line, -1); matches != nil {
			for _, match := range matches {
				start, end := match[0], match[1]
				if !s.isRangeCovered(covered, start, end) {
					tokens = append(tokens, GMLToken{
						Start:     start,
						Length:    end - start,
						TokenType: SemanticTokenTypeComment,
						Modifiers: 0,
						Line:      lineNum,
						Character: start,
					})
					s.markRangeCovered(covered, start, end)
				}
			}
		}

		// Process strings (second highest precedence)
		if matches := patterns[SemanticTokenTypeString].FindAllStringIndex(line, -1); matches != nil {
			for _, match := range matches {
				start, end := match[0], match[1]
				if !s.isRangeCovered(covered, start, end) {
					tokens = append(tokens, GMLToken{
						Start:     start,
						Length:    end - start,
						TokenType: SemanticTokenTypeString,
						Modifiers: 0,
						Line:      lineNum,
						Character: start,
					})
					s.markRangeCovered(covered, start, end)
				}
			}
		}

		// Process other token types
		for tokenType, pattern := range patterns {
			if tokenType == SemanticTokenTypeComment || tokenType == SemanticTokenTypeString {
				continue // Already processed
			}
			matches := pattern.FindAllStringSubmatchIndex(line, -1)
			for _, match := range matches {
				start, end := match[0], match[1]
				if !s.isRangeCovered(covered, start, end) {
					// Determine if it's a built-in function
					modifier := 0
					if tokenType == SemanticTokenTypeFunction {
						funcName := line[start:end]
						if _, exists := s.functions[funcName]; exists {
							modifier = 1 << SemanticTokenModifierDefaultLibrary
						}
					} else if tokenType == SemanticTokenTypeVariable {
						varName := line[start:end]
						if _, exists := s.variables[varName]; exists {
							modifier = 1 << SemanticTokenModifierDefaultLibrary
						}
					} else if tokenType == SemanticTokenTypeProperty {
						constName := line[start:end]
						if _, exists := s.constants[constName]; exists {
							modifier = 1 << SemanticTokenModifierDefaultLibrary
						}
					}
					tokens = append(tokens, GMLToken{
						Start:     start,
						Length:    end - start,
						TokenType: tokenType,
						Modifiers: modifier,
						Line:      lineNum,
						Character: start,
					})
					s.markRangeCovered(covered, start, end)
				}
			}
		}

		// Process user-defined variables (identifiers that aren't keywords, functions, constants, or built-in variables)
		matches := userVariablePattern.FindAllStringIndex(line, -1)
		for _, match := range matches {
			start, end := match[0], match[1]
			if !s.isRangeCovered(covered, start, end) {
				identifier := line[start:end]

				// Skip if it's already a known token type
				if _, exists := s.keywords[identifier]; exists {
					continue
				}
				if _, exists := s.functions[identifier]; exists {
					continue
				}
				if _, exists := s.constants[identifier]; exists {
					continue
				}
				if _, exists := s.variables[identifier]; exists {
					continue
				}

				// This is likely a user-defined variable
				tokens = append(tokens, GMLToken{
					Start:     start,
					Length:    end - start,
					TokenType: SemanticTokenTypeVariable,
					Modifiers: 0, // No modifier for user-defined variables
					Line:      lineNum,
					Character: start,
				})
				s.markRangeCovered(covered, start, end)
			}
		}
	}

	// Sort tokens by line and character position
	sort.Slice(tokens, func(i, j int) bool {
		if tokens[i].Line == tokens[j].Line {
			return tokens[i].Character < tokens[j].Character
		}
		return tokens[i].Line < tokens[j].Line
	})

	return tokens
}

// Helper function to get keys from a map
func (s *GMLLanguageServer) getKeys(m map[string]CompletionItem) []string {
	keys := make([]string, 0, len(m))
	for k := range m {
		keys = append(keys, regexp.QuoteMeta(k))
	}
	return keys
}

func (s *GMLLanguageServer) isRangeCovered(covered []bool, start, end int) bool {
	for i := start; i < end && i < len(covered); i++ {
		if covered[i] {
			return true
		}
	}
	return false
}

func (s *GMLLanguageServer) markRangeCovered(covered []bool, start, end int) {
	for i := start; i < end && i < len(covered); i++ {
		covered[i] = true
	}
}

func (s *GMLLanguageServer) tokensToLSPData(tokens []GMLToken) []int {
	data := make([]int, 0)
	lastLine := 0
	lastChar := 0

	for _, token := range tokens {
		// Delta line
		deltaLine := token.Line - lastLine

		// Delta character (if same line, relative to last char; if new line, absolute)
		deltaChar := token.Character
		if deltaLine == 0 {
			deltaChar = token.Character - lastChar
		}

		// Append: deltaLine, deltaChar, length, tokenType, tokenModifiers
		data = append(data, deltaLine, deltaChar, token.Length, token.TokenType, token.Modifiers)

		lastLine = token.Line
		lastChar = token.Character
	}

	return data
}

func (s *GMLLanguageServer) emptyCompletion(msg *Message) *Message {
	return &Message{
		Jsonrpc: "2.0",
		ID:      msg.ID,
		Result: CompletionList{
			IsIncomplete: false,
			Items:        []CompletionItem{},
		},
	}
}

func main() {
	rootPath := flag.String("root", ".", "Root directory of GML files")
	flag.Parse()

	server := NewGMLLanguageServer(*rootPath)

	// Redirect log output to a file for debugging
	f, err := os.OpenFile("/tmp/gml-lsp.log", os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0666)
	if err != nil {
		log.Fatalf("error opening file: %v", err)
	}
	defer f.Close()
	log.SetOutput(f)
	log.Println("GML Language Server starting...")

	reader := bufio.NewReader(os.Stdin)
	for {
		// Read Content-Length header
		line, err := reader.ReadString('\n')
		if err != nil {
			if err != io.EOF {
				log.Printf("Error reading header: %v", err)
			}
			return
		}
		line = strings.TrimSpace(line)
		if !strings.HasPrefix(line, "Content-Length:") {
			log.Printf("Unexpected header: %s", line)
			continue
		}
		lengthStr := strings.TrimSpace(strings.TrimPrefix(line, "Content-Length:"))
		length, err := strconv.Atoi(lengthStr)
		if err != nil {
			log.Printf("Could not parse content length: %v", err)
			continue
		}
		// Read the empty line separator
		_, err = reader.ReadString('\n')
		if err != nil {
			log.Printf("Error reading separator: %v", err)
			return
		}
		// Read message content
		content := make([]byte, length)
		if _, err := io.ReadFull(reader, content); err != nil {
			log.Printf("Could not read content: %v", err)
			return
		}
		log.Printf("Received message: %s", string(content))
		// Parse JSON-RPC message
		var msg Message
		if err := json.Unmarshal(content, &msg); err != nil {
			log.Printf("Could not unmarshal message: %v", err)
			continue
		}
		// Handle message
		response := server.handleMessage(&msg)
		// Send response if not nil
		if response != nil {
			responseBytes, err := json.Marshal(response)
			if err != nil {
				log.Printf("Could not marshal response: %v", err)
				continue
			}
			log.Printf("Sending response: %s", string(responseBytes))
			fmt.Printf("Content-Length: %d\r\n\r\n%s", len(responseBytes), responseBytes)
		}
	}
}
