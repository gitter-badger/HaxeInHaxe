import Ast;
import Type;

enum Package_rule {
	Forbidden;
	Directory(value: String);
	Remap(value: String);
};

typedef Pos = Ast.Pos;

typedef Basic_types = {
	tvoid : T,
	tint : T,
	tfloat : T,
	tbool : T,
	tnull : T -> T,
	tstring : T,
	tarray : T -> T
};

typedef Stats = {
	s_files_parsed : Ref<Int>,
	s_classes_built : Ref<Int>,
	s_methods_typed : Ref<Int>,
	s_macros_called : Ref<Int>
};

enum Platform {
	Cross;
	Js;
	Neko;
	Flash;
	Php;
	Cpp;
	Cs;
	Java;
	Python;
};

enum Capture_policy {
	CPNone;
	CPWrapRef;
	CPLoopVars;
};

typedef Platform_config = {
	pf_static : Bool,
	pf_sys : Bool,
	pf_locals_scope : Bool,
	pf_captured_scope : Bool,
	pf_unique_locals : Bool,
	pf_capture_policy : Capture_policy,
	pf_pad_nulls : Bool,
	pf_add_final_return : Bool,
	pf_overload : Bool,
	pf_pattern_matching : Bool,
	pf_can_skip_non_nullable_argument : Bool,
	pf_reserved_type_paths : List<Path>
};

enum Display_mode {
	DMNone;
	DMDefault;
	DMUsage;
	DMPosition;
	DMToplevel;
	DMResolve(value: String);
	DMType;
};

typedef Context = {
	version : Int,
	args : List<String>,
	sys_args : List<String>,
	display : Display_mode,
	debug : Bool,
	verbose : Bool,
	foptimize : Bool,
	platform : Platform,
	config : Platform_config,
	std_path : List<String>,
	class_path : List<String>,
	main_class : Option<Type.Path>,
	defines : PMap<String, String>,
	package_rules : PMap<String, Package_rule>,
	error : String -> Pos -> Unit,
	warning : String -> Pos -> Unit,
	load_extern_type : List < Path -> Pos -> Option<Tuple<String, Ast.Package> >>,
	filters : List < Unit -> Unit >,
	final_filters : List < Unit -> Unit >,
	defines_signature : Option<String>,
	print : String -> Unit,
	get_macros : Unit -> Option<Context>,
	run_command : String -> Int,
	file_lookup_cache : Hashtbl<String, String>,
	stored_typed_exprs : PMap<Int, Texpr>,
	file : String,
	flash_version : Float,
	features : Hashtbl<String, Bool>,
	modules : List<Type.Module_def>,
	main : Option<Type.Texpr>,
	types : List<Type.Module_type>,
	resources : Hashtbl<String, String>,
	neko_libs : List<String>,
	include_files : List<Tuple<String, String>>,
	php_front : Option<String>,
	php_lib : Option<String>,
	php_prefix : Option<String>,
	swf_libs : List < Tuple < String, Unit -> Swf.Swf, Unit -> Hashtbl<Tuple<List<String>, String>, As3hl.Hl_class >>>,
	java_libs : List < Tuple < String, Bool, Unit -> Unit, Unit -> List<Path>, Path -> Option<Tuple<JData.Jclass, String, String> >>>,
	net_libs : List < Tuple < String, Bool, Unit -> List<Path>, Path -> Option<IlData.Ilclass >>>,
	net_std : List<String>,
	net_path_map : Hashtbl<Path, Tuple<List<String>, List<String>, String>>,
	c_args : List<String>,
	js_gen : Option < Unit -> Unit >,
	basic : Basic_types,
	memory_marker : Array<Float>
};

class /*exception*/ Abort {

};

typedef Timer_infos = {
	name : String,
	start : List<Float>,
	total : Float
};
class Common {
	public static var display_default = ref(DMNone);

	public static var stats = { () with s_files_parsed = ref(0);
								s_classes_built = ref(0);
								s_methods_typed = ref(0);
								s_macros_called = ref(0)
							  };

	public static var default_config = { () with pf_static = True;
										 pf_sys = True;
										 pf_locals_scope = True;
										 pf_captured_scope = True;
										 pf_unique_locals = False;
										 pf_capture_policy = CPNone;
										 pf_pad_nulls = False;
										 pf_add_final_return = False;
										 pf_overload = False;
										 pf_pattern_matching = False;
										 pf_can_skip_non_nullable_argument = True;
										 pf_reserved_type_paths = []
									   };

	public static function get_config(com) return {
		function defined(f) return {
			PMap.mem(fst(Define.infos(f)), com.defines);
		};
		switch (com.platform) {
		case Cross: default_config;
		case Js: {
			() with pf_static = False;
			pf_sys = False;
			pf_locals_scope = False;
			pf_captured_scope = False;
			pf_unique_locals = False;
			pf_capture_policy = CPLoopVars;
			pf_pad_nulls = False;
			pf_add_final_return = False;
			pf_overload = False;
			pf_pattern_matching = False;
			pf_can_skip_non_nullable_argument = True;
			pf_reserved_type_paths = ::((new Tuple([], "Object")), ::((new Tuple([], "Error")), []))
		};
		case Neko: {
			() with pf_static = False;
			pf_sys = True;
			pf_locals_scope = True;
			pf_captured_scope = True;
			pf_unique_locals = False;
			pf_capture_policy = CPNone;
			pf_pad_nulls = True;
			pf_add_final_return = False;
			pf_overload = False;
			pf_pattern_matching = False;
			pf_can_skip_non_nullable_argument = True;
			pf_reserved_type_paths = []
		};
		case Flash if (defined(Define.As3)): {
			() with pf_static = True;
			pf_sys = False;
			pf_locals_scope = False;
			pf_captured_scope = True;
			pf_unique_locals = True;
			pf_capture_policy = CPLoopVars;
			pf_pad_nulls = False;
			pf_add_final_return = True;
			pf_overload = False;
			pf_pattern_matching = False;
			pf_can_skip_non_nullable_argument = False;
			pf_reserved_type_paths = []
		};
		case Flash: {
			() with pf_static = True;
			pf_sys = False;
			pf_locals_scope = True;
			pf_captured_scope = True;
			pf_unique_locals = False;
			pf_capture_policy = CPLoopVars;
			pf_pad_nulls = False;
			pf_add_final_return = False;
			pf_overload = False;
			pf_pattern_matching = False;
			pf_can_skip_non_nullable_argument = False;
			pf_reserved_type_paths = ::((new Tuple([], "Object")), ::((new Tuple([], "Error")), []))
		};
		case Php: {
			() with pf_static = False;
			pf_sys = True;
			pf_locals_scope = False;
			pf_captured_scope = False;
			pf_unique_locals = False;
			pf_capture_policy = CPNone;
			pf_pad_nulls = True;
			pf_add_final_return = False;
			pf_overload = False;
			pf_pattern_matching = False;
			pf_can_skip_non_nullable_argument = True;
			pf_reserved_type_paths = []
		};
		case Cpp: {
			() with pf_static = True;
			pf_sys = True;
			pf_locals_scope = True;
			pf_captured_scope = True;
			pf_unique_locals = False;
			pf_capture_policy = CPWrapRef;
			pf_pad_nulls = True;
			pf_add_final_return = True;
			pf_overload = False;
			pf_pattern_matching = False;
			pf_can_skip_non_nullable_argument = True;
			pf_reserved_type_paths = []
		};
		case Cs: {
			() with pf_static = True;
			pf_sys = True;
			pf_locals_scope = False;
			pf_captured_scope = True;
			pf_unique_locals = True;
			pf_capture_policy = CPWrapRef;
			pf_pad_nulls = True;
			pf_add_final_return = False;
			pf_overload = True;
			pf_pattern_matching = False;
			pf_can_skip_non_nullable_argument = True;
			pf_reserved_type_paths = []
		};
		case Java: {
			() with pf_static = True;
			pf_sys = True;
			pf_locals_scope = False;
			pf_captured_scope = True;
			pf_unique_locals = False;
			pf_capture_policy = CPWrapRef;
			pf_pad_nulls = True;
			pf_add_final_return = False;
			pf_overload = True;
			pf_pattern_matching = False;
			pf_can_skip_non_nullable_argument = True;
			pf_reserved_type_paths = []
		};
		case Python: {
			() with pf_static = False;
			pf_sys = True;
			pf_locals_scope = False;
			pf_captured_scope = False;
			pf_unique_locals = False;
			pf_capture_policy = CPLoopVars;
			pf_pad_nulls = False;
			pf_add_final_return = False;
			pf_overload = False;
			pf_pattern_matching = False;
			pf_can_skip_non_nullable_argument = True;
			pf_reserved_type_paths = []
		};
		};
	};

	public static var memory_marker = [Unix.time([])];

	public static function create(v, args) return {
		var m = Type.mk_mono([]);
		var defines = PMap.add("true", "1", PMap.add("source-header", "Generated by Haxe", if (<>(display_default.val, DMNone)) {
		PMap.add("display", "1", PMap.empty);
		} else {
			PMap.empty;
		}));
		{
			() with version = v;
			args = args;
			sys_args = args;
			debug = False;
			display = display_default.val;
			verbose = False;
			foptimize = True;
			features = Hashtbl.create(0);
			platform = Cross;
			config = default_config;
			print = function s: print_string(s);
			flush(stdout);
			run_command = Sys.command;
			std_path = [];
			class_path = [];
			main_class = None;
			defines = defines;
			package_rules = PMap.empty;
			file = "";
			types = [];
			filters = [];
			final_filters = [];
			modules = [];
			main = None;
			flash_version = 10.;
			resources = Hashtbl.create(0);
			php_front = None;
			php_lib = None;
			swf_libs = [];
			java_libs = [];
			net_libs = [];
			net_std = [];
			net_path_map = Hashtbl.create(0);
			c_args = [];
			neko_libs = [];
			include_files = [];
			php_prefix = None;
			js_gen = None;
			load_extern_type = [];
			defines_signature = None;
			get_macros = function []: None;
			warning = function _: function _: assert False;
			error = function _: function _: assert False;
			basic = {
				() with tvoid = m;
				tint = m;
				tfloat = m;
				tbool = m;
				tnull = function _: assert False;
				tstring = m;
				tarray = function _: assert False
			};
			file_lookup_cache = Hashtbl.create(0);
			stored_typed_exprs = PMap.empty;
			memory_marker = memory_marker
		};
	};

	public static function log(com, str) return {
		if (com.verbose) {
			com.print( ^ (str, "\n"));
		} else {
			[];
		};
	};

	public static function clone(com) return {
		var t = com.basic;
		{
			(com) with basic = { (t) with tvoid = t.tvoid };
			main_class = None;
			features = Hashtbl.create(0);
			file_lookup_cache = Hashtbl.create(0)
		};
	};

	public static function file_time(file) return {
		try {
			Unix.stat(file).Unix.st_mtime;
		} catch (e: _) {
			0.;
		};
	};

	public static function get_signature(com) return {
		switch (com.defines_signature) {
		case Some(s): s;
		case None: var str = String.concat("@", PMap.foldi(function k: function v: function acc: switch (k) {
		case display | use_rtti_doc | macrotimes: acc;
		case _: ::(k, ::(v, acc));
			}, com.defines, []));
			var s = Digest.string(str);
			com.defines_signature = Some(s);
			s;
		};
	};

	public static function file_extension(file) return {
		switch (List.rev(ExtString.String.nsplit(file, "."))) {
		case ::(e, _): String.lowercase(e);
		case []: "";
		};
	};

	public static var platforms = ::(Js, ::(Neko, ::(Flash, ::(Php, ::(Cpp, ::(Cs, ::(Java, ::(Python, []))))))));

	public static function platform_name(match) return switch (match) {
	case Cross: "cross";
	case Js: "js";
	case Neko: "neko";
	case Flash: "flash";
	case Php: "php";
	case Cpp: "cpp";
	case Cs: "cs";
	case Java: "java";
	case Python: "python";
	};

	public static var flash_versions = List.map(function v: var maj = int_of_float(v);
									   var min = int_of_float(mod_float( * .(v, 10.), 10.));
	(new Tuple(v, ^ (string_of_int(maj), if ( = (min, 0)) {
	"";
} else {
	^ ("_", string_of_int(min));
	}))), ::(9., ::(10., ::(10.1, ::(10.2, ::(10.3, ::(11., ::(11.1, ::(11.2, ::(11.3, ::(11.4, ::(11.5, ::(11.6, ::(11.7,
									 ::(11.8, ::(11.9, ::(12.0, ::(13.0, ::(14.0, ::(15.0, ::(16.0, ::(17.0, []))))))))))))))))))))));

	public static function flash_version_tag(match) return switch (match) {
	case undefined: 6;
	case undefined: 7;
	case undefined: 8;
	case undefined: 9;
	case undefined | undefined: 10;
	case undefined: 11;
	case undefined: 12;
	case undefined: 13;
	case undefined: 14;
	case undefined: 15;
	case undefined: 16;
	case undefined: 17;
	case undefined: 18;
	case undefined: 19;
	case undefined: 20;
	case undefined: 21;
	case undefined: 22;
	case undefined: 23;
	case undefined: 24;
	case undefined: 25;
	case undefined: 26;
	case undefined: 27;
	case undefined: 28;
	case v: failwith( ^ ("Invalid SWF version ", string_of_float(v)));
	};

	public static function raw_defined(ctx, v) return {
		PMap.mem(v, ctx.defines);
	};

	public static function defined(ctx, v) return {
		raw_defined(ctx, fst(Define.infos(v)));
	};

	public static function raw_defined_value(ctx, k) return {
		PMap.find(k, ctx.defines);
	};

	public static function defined_value(ctx, v) return {
		raw_defined_value(ctx, fst(Define.infos(v)));
	};

	public static function defined_value_safe(ctx, v) return {
		try {
			defined_value(ctx, v);
		} catch (e: Not_found) {
			"";
		};
	};

	public static function raw_define(ctx, v) return {
		var Tuple(k, v) = try {
			ExtString.String.split(v, "=");
		} catch (e: _) {
			(new Tuple(v, "1"));
		};
		ctx.defines = PMap.add(k, v, ctx.defines);
		var k = String.concat("_", ExtString.String.nsplit(k, "-"));
		ctx.defines = PMap.add(k, v, ctx.defines);
		ctx.defines_signature = None;
	};

	public static function define_value(ctx, k, v) return {
		raw_define(ctx, ^ (fst(Define.infos(k)), ^ ("=", v)));
	};

	public static function define(ctx, v) return {
		raw_define(ctx, fst(Define.infos(v)));
	};

	public static function init_platform(com, pf) return {
		com.platform = pf;
		var name = platform_name(pf);
		function forbid(acc, p) return {
			if ( || ( = (p, name), PMap.mem(p, acc))) {
				acc;
			} else {
				PMap.add(p, Forbidden, acc);
			};
		};
		com.package_rules = List.fold_left(forbid, com.package_rules, List.map(platform_name, platforms));
		com.config = get_config(com);
		if (com.config.pf_sys) {
			define(com, Define.Sys);
		} else {
			com.package_rules = PMap.add("sys", Forbidden, com.package_rules);
		};
		raw_define(com, name);
	};

	public static function add_feature(com, f) return {
		Hashtbl.replace(com.features, f, True);
	};

	public static function has_dce(com) return {
		try {
			<>(defined_value(com, Define.Dce), "no");
		} catch (e: Not_found) {
			False;
		};
	};

	public static function is_directly_used(com, meta) return {
		|| (!(has_dce(com)), Ast.Meta.has(Ast.Meta.DirectlyUsed, meta));
	};

	public static function has_feature(com, f) return {
		try {
			Hashtbl.find(com.features, f);
		} catch (e: Not_found) {
			if ( = (com.types, [])) {
				!(has_dce(com));
			} else {
				switch (List.rev(ExtString.String.nsplit(f, "."))) {
				case []: assert False;
				case ::(cl, []): has_feature(com, ^ (cl, ".*"));
				case ::(meth, ::(cl, pack)): var r = try {
						var path = (new Tuple(List.rev(pack), cl));
						switch (List.find(function t:
										  && ( = (t_path(t), path), !(Ast.Meta.has(Ast.Meta.RealPath, t_infos(t).mt_meta))), com.types)) {
						case t if (=(meth, "*")):
							switch (t) {
							case TAbstractDecl(a): Ast.Meta.has(Ast.Meta.ValueUsed, a.a_meta);
							case _: Ast.Meta.has(Ast.Meta.Used, t_infos(t).mt_meta);
							};
						case TClassDecl({ cl_extern = True } = c) if (||(<>(com.platform, Js), &&(<>(cl, "Array"), <>(cl, "Math")))): Meta.has(
							Meta.Used, try {
								PMap.find(meth, c.cl_statics);
							} catch (e: Not_found) {
								PMap.find(meth, c.cl_fields);
							} .cf_meta);
						case TClassDecl(c): || (PMap.exists(meth, c.cl_statics), PMap.exists(meth, c.cl_fields));
						case _: False;
						};
					} catch (e: Not_found) {
						False;
					};
					var r = || (r, !(has_dce(com)));
					Hashtbl.add(com.features, f, r);
					r;
				};
			};
		};
	};

	public static function allow_package(ctx, s) return {
		try {
			if ( = (PMap.find(s, ctx.package_rules), Forbidden)) {
				ctx.package_rules = PMap.remove(s, ctx.package_rules);
			} else {
				[];
			};
		} catch (e: Not_found) {
			[];
		};
	};

	public static function error(msg, p) return {
		raise(Abort(msg, p));
	};

	public static function platform(ctx, p) return {
		= (ctx.platform, p);
	};

	public static function add_filter(ctx, f) return {
		ctx.filters = ::(f, ctx.filters);
	};

	public static function add_final_filter(ctx, f) return {
		ctx.final_filters = ::(f, ctx.final_filters);
	};

	public static function find_file(ctx, f) return {
		try {
			switch (Hashtbl.find(ctx.file_lookup_cache, f)) {
			case None: raise(Exit);
			case Some(f): f;
			};
		} catch (e: T) {
			McOr(McArr(PaId(IdUid(Exit)), ExNil, ExApp(ExId(IdLid(<...>)), ExId(IdUid(<...>)))), McArr(PaId(IdUid(Not_found)), ExNil,
					ExLet(ReRecursive, BiEq(PaId(<...>), ExFun(<...>)), ExLet(ReNil, BiEq(<...>, <...>),
						ExSeq(<...>)))))			case Exit: raise(Not_found);
		case Not_found: 	function loop(had_empty) return {
			case [] if (had_empty): raise(Not_found);
			case []: loop(True, ::("", []));
			case ::(p, l): var file = ^ (p, f);
				if (Sys.file_exists(file)) {
					file;
				} else {
					loop( || (had_empty, = (p, "")), l);
				};
			};
			var r = try {
				Some(loop(False, ctx.class_path));
			} catch (e: Not_found) {
				None;
			};
			Hashtbl.add(ctx.file_lookup_cache, f, r);
			switch (r) {
			case None: raise(Not_found);
			case Some(f): f;
			};
		};
	};

	public static function get_full_path(f) return {
		try {
			Extc.get_full_path(f);
		} catch (e: _) {
			f;
		};
	};

	public static var unique_full_path = if ( || ( = (Sys.os_type, "Win32"), = (Sys.os_type, "Cygwin"))) {
		function f: String.lowercase(get_full_path(f));
	} else {
		get_full_path;
	};

	public static function normalize_path(p) return {
		var l = String.length(p);
		if ( = (l, 0)) {
			"./";
		} else {
			switch (p - (l, 1)) {
			case '\\' | '/': p;
			case _: ^ (p, "/");
			};
		};
	};

	public static function mkdir_recursive(base, dir_list) return {
		switch (dir_list) {
		case []: [];
		case ::(dir, remaining): var path = switch (base) {
			case : dir;
			case /: ^ ("/", dir);
			case _: ^ (base, ^ ("/", dir));
			};
			if (!( || ( = (path, ""), && ( = (String.length(path), 2), = (String.sub(path, 1, 1), ":"))))) {
				if (!(Sys.file_exists(path))) {
					Unix.mkdir(path, 0o755);
				} else {
					[];
				};
			} else {
				[];
			};
			mkdir_recursive(if ( = (path, "")) {
			"/";
		} else {
			path;
		}, remaining);
		};
	};

	public static function mkdir_from_path(path) return {
		var parts = Str.split_delim(Str.regexp("[\\/]+"), path);
		switch (parts) {
		case []: [];
		case _: var dir_list = List.rev(List.tl(List.rev(parts)));
			mkdir_recursive("", dir_list);
		};
	};

	public static function mem_size(v) return {
		Objsize.size_with_headers(Objsize.objsize(v, [], []));
	};

	public static var get_time = Extc.time;

	public static var htimers = Hashtbl.create(0);

	public static function new_timer(name) return {
		try {
			var t = Hashtbl.find(htimers, name);
			t.start = ::(get_time([]), t.start);
			t;
		} catch (e: Not_found) {
			var t = {
				() with name = name;
				start = ::(get_time([]), []);
				total = 0.
			};
			Hashtbl.add(htimers, name, t);
			t;
		};
	};

	public static var curtime = ref([]);

	public static function close(t) return {
		var start = switch (t.start) {
		case []: assert False;
		case ::(s, l): t.start = l;
			s;
		};
		var now = get_time([]);
		var dt = -.(now, start);
		t.total = +.(t.total, dt);
		function loop([]) return {
			switch (curtime.val) {
			case []: failwith( ^ ("Timer ", ^ (t.name, " closed while not active")));
			case ::(tt, l): curtime.val = l;
				if ( != (t, tt)) {
					loop([]);
				} else {
					[];
				};
			};
		};
		loop([]);
		List.iter(function ct: ct.start = List.map(function t: var s = +.(t, dt);
		if ( > (s, now)) {
		now;
	} else {
		s;
	}, ct.start), curtime.val);
	};

	public static function timer(name) return {
		var t = new_timer(name);
		curtime.val = ::(t, curtime.val);
		function []: close(t);
	};

	public static function close_times([]) return {
		switch (curtime.val) {
		case []: [];
		case ::(t, _): close(t);
			close_times([]);
		};
	};

	public static function __init__() {
		Ast.Meta.to_string_ref.val = function m: fst(MetaInfo.to_string(m));
	};

	public static function valid_float_lexeme(s) return {
		var l = String.length(s);
		function loop(i) return {
			if ( >= (i, l)) {
				^ (s, ".");
			} else {
				switch (si) {
				case '0' .. '9' | '-': loop(+(i, 1));
				case _: s;
				};
			};
		};
		loop(0);
	};

	public static function float_repres(f) return {
		switch (classify_float(f)) {
		case FP_nan: "nan";
		case FP_infinite: if ( < (f, 0.0)) {
				"neg_infinity";
			} else {
				"infinity";
			};
		case _: var float_val = var s1 = Printf.sprintf("%.12g", f);
			if ( = (f, float_of_string(s1))) {
				s1;
			} else {
				var s2 = Printf.sprintf("%.15g", f);
				if ( = (f, float_of_string(s2))) {
					s2;
				} else {
					Printf.sprintf("%.18g", f);
				};
			};
			valid_float_lexeme(float_val);
		};
	}
}
;
