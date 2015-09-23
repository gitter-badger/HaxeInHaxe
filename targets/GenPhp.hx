import Ast;
import Type;
import Common;

typedef Method_name = {
	mpath : Path,
	mname : String
};

typedef Inline_method = {
	iname : String,
	iindex : Int,
	iexpr : Texpr,
	ihasthis : Bool,
	iin_block : Bool,
	iarguments : List<String>,
	ilocals : PMap<String, String>,
	iinv_locals : PMap<String, String>
};

typedef Context = {
	com : Common.Context,
	ch : Out_channel,
	buf : Buffer,
	path : Path,
	stack : Codegen.Stack_context,
	nested_loops : Int,
	inline_index : Int,
	curclass : Tclass,
	curmethod : String,
	tabs : String,
	in_value : Option<String>,
	in_loop : Bool,
	in_block : Bool,
	in_instance_method : Bool,
	imports : Hashtbl<String, List<String>>,
	extern_required_paths : List<Tuple<List<String>, String>>,
	extern_classes_with_init : List<Path>,
	locals : PMap<String, String>,
	inv_locals : PMap<String, String>,
	local_types : List<T>,
	inits : List<Texpr>,
	constructor_block : Bool,
	all_dynamic_methods : List<Method_name>,
	dynamic_methods : List<Tclass_field>,
	is_call : Bool,
	cwd : String,
	inline_methods : List<Inline_method>,
	lib_path : String
};
class Genphp {
	public static function join_class_path(path, separator) return {
		var result = switch ((new Tuple(fst(path), snd(path)))) {
		case ([], s): s;
		case (el, s): ^ (String.concat(separator, el), ^ (separator, s));
		};
		if (String.contains(result, '+')) {
			var idx = String.index(result, '+');
			^ (String.sub(result, 0, idx), String.sub(result, +(idx, 1), -(-(String.length(result), idx), 1)));
		} else {
			result;
		};
	};

	public static function class_string(klass, suffix, params) return {
		switch (klass.cl_path) {
		case ([], Array): ^ (snd(klass.cl_path), ^ (suffix, ^ ("<", ^ (String.concat(",", List.map(type_string, params)), " >"))));
		case _ if (switch (klass.cl_kind) {
				case KTypeParameter(_): True;
					case _: False;
					}): "Dynamic";
		case ([], #Int): "/* # */int";
		case (::(haxe, ::(io, [])), Unsigned_char__): "unsigned char";
		case ([], Class): "Class";
		case ([], Null): switch (params) {
			case ::(t, []): switch (follow(t)) {
				case TInst({ cl_path = ([], Int) }, _) | TInst({ cl_path = ([], Float) }, _) | TEnum({ e_path = ([], Bool) }, _):
					"Dynamic";
				case _: ^ ("/*NULL*/", type_string(t));
				};
			case _: assert False;
			};
		case _: ^ (join_class_path(klass.cl_path, "::"), suffix);
		};
	};

	public static function type_string_suff(suffix, haxe_type) return {
		switch (haxe_type) {
		case TMono(r): switch (r.val) {
			case None: "Dynamic";
			case Some(t): type_string_suff(suffix, t);
			};
		case TAbstract({ a_path = ([], Int) }, []): "int";
		case TAbstract({ a_path = ([], Float) }, []): "double";
		case TAbstract({ a_path = ([], Bool) }, []): "bool";
		case TAbstract({ a_path = ([], Void) }, []): "Void";
		case TEnum(enum, params): ^ (join_class_path(enum.e_path, "::"), suffix);
		case TInst(klass, params): class_string(klass, suffix, params);
		case TAbstract(abs, params): ^ (join_class_path(abs.a_path, "::"), suffix);
		case TType(type_def, params): switch (type_def.t_path) {
			case ([], Null): switch (params) {
				case ::(t, []): switch (follow(t)) {
					case TInst({ cl_path = ([], Int) }, _) | TInst({ cl_path = ([], Float) }, _) | TEnum({ e_path = ([], Bool) }, _):
						"Dynamic";
					case _: type_string_suff(suffix, t);
					};
				case _: assert False;
				};
			case ([], Array): switch (params) {
				case ::(t, []): ^ ("Array<", ^ (type_string(follow(t)), " >"));
				case _: assert False;
				};
			case _: type_string_suff(suffix, apply_params(type_def.t_params, params, type_def.t_type));
			};
		case TFun(args, haxe_type): "Dynamic";
		case TAnon(anon): "Dynamic";
		case TDynamic(haxe_type): "Dynamic";
		case TLazy(func): type_string_suff(suffix, func.val([]));
		};
	};

	public static function type_string(haxe_type) return {
		type_string_suff("", haxe_type);
	};

	public static function debug_expression(expression, type_too) return {
		^ ("/* ", ^ (Type.s_expr_kind(expression), ^ (if (type_too) {
		^ (" = ", type_string(follow(expression.etype)));
		} else {
			"";
		}, " */")));
	};

	public static function register_extern_required_path(ctx, path) return {
		if ( && (List.exists(function p: = (p, path), ctx.extern_classes_with_init), !(List.exists(function p: = (p, path), ctx.extern_required_paths)))) {
			ctx.extern_required_paths = ::(path, ctx.extern_required_paths);
		} else {
			[];
		};
	};

	public static var s_expr_expr = Type.s_expr_kind;

	public static function s_expr_name(e) return {
		s_type(print_context([]), follow(e.etype));
	};

	public static function s_type_name(t) return {
		s_type(print_context([]), t);
	};

	public static function start_with(s, test) return {
		var len = String.length(test);
		&& ( > (String.length(s), len), = (String.sub(s, 0, len), test));
	};

	public static function is_uncertain_type(t) return {
		switch (follow(t)) {
		case TInst(c, _): c.cl_interface;
		case TMono(_): True;
		case TAnon(a): switch (a.a_status.val) {
			case Statics(_) | EnumStatics(_): False;
			case _: True;
			};
		case TDynamic(_): True;
		case _: False;
		};
	};

	public static function is_uncertain_expr(e) return {
		is_uncertain_type(e.etype);
	};

	public static function is_anonym_type(t) return {
		switch (follow(t)) {
		case TAnon(a): switch (a.a_status.val) {
			case Statics(_) | EnumStatics(_): False;
			case _: True;
			};
		case TDynamic(_): True;
		case _: False;
		};
	};

	public static function is_anonym_expr(e) return {
		is_anonym_type(e.etype);
	};

	public static function is_unknown_type(t) return {
		switch (follow(t)) {
		case TMono(r): switch (r.val) {
			case None: True;
			case Some(t): is_unknown_type(t);
			};
		case _: False;
		};
	};

	public static function is_unknown_expr(e) return {
		is_unknown_type(e.etype);
	};

	public static function is_string_type(t) return {
		switch (follow(t)) {
		case TInst({ cl_path = ([], String) }, _): True;
		case TAnon(a): switch (a.a_status.val) {
			case Statics({ cl_path = ([], String) }): True;
			case _: False;
			};
		case TAbstract(a, pl): is_string_type(Abstract.get_underlying_type(a, pl));
		case _: False;
		};
	};

	public static function is_string_expr(e) return {
		is_string_type(e.etype);
	};

	public static function to_string(ctx, e) return {
		var v = alloc_var("__call__", t_dynamic);
		var f = mk(TLocal(v), t_dynamic, e.epos);
		mk(TCall(f, ::(Codegen.string(ctx.com, "_hx_string_rec", e.epos), ::(e, ::(Codegen.string(ctx.com, "", e.epos), [])))), ctx.com.basic.tstring, e.epos);
	};

	public static function as_string_expr(ctx, e) return {
		switch (e.eexpr) {
		case TConst(TNull): to_string(ctx, e);
		case _ if (!(is_string_expr(e))): to_string(ctx, e);
		case _: e;
		};
	};

	public static function to_string_null(ctx, e) return {
		var v = alloc_var("__call__", t_dynamic);
		var f = mk(TLocal(v), t_dynamic, e.epos);
		mk(TCall(f, ::(Codegen.string(ctx.com, "_hx_string_or_null", e.epos), ::(e, []))), ctx.com.basic.tstring, e.epos);
	};

	public static function as_string_expr(ctx, e) return {
		switch (e.eexpr) {
		case TConst(TNull): to_string(ctx, e);
		case TConst(TString(s)): e;
		case TBinop(op, _, _) if (is_string_expr(e)): e;
		case TCall({ eexpr = TField({ eexpr = TTypeExpr(TClassDecl({ cl_path = ([], Std) })) }, FStatic(c, f)) }, ::(_, [])) if (=(f.cf_name, "string"))
				: e;
		case TCall({ eexpr = TLocal(_) }, ::({ eexpr = TConst(TString(_hx_string_rec | _hx_str_or_null)) }, [])): e;
		case _ if (!(is_string_expr(e))): to_string(ctx, e);
		case _: to_string_null(ctx, e);
		};
	};

	public static function spr(ctx, s) return {
		Buffer.add_string(ctx.buf, s);
	};

	public static function print(ctx) return {
		Printf.kprintf(function s: Buffer.add_string(ctx.buf, s));
	};

	public static function prefix_class(com, name) return {
		switch (com.php_prefix) {
		case Some(prefix_class): ^ (prefix_class, name);
		case _: name;
		};
	};

	public static function prefix_init_replace(com, code) return {
		var r = Str.regexp("php_Boot");
		Str.global_replace(r, ^ ("php_", prefix_class(com, "Boot")), code);
	};

	public static function s_path(ctx, path, isextern, p) return {
		if (isextern) {
			register_extern_required_path(ctx, path);
			snd(path);
		} else {
			switch (path) {
			case ([], List): prefix_class(ctx.com, "HList");
			case ([], name): prefix_class(ctx.com, name);
			case (pack, name): try {
					switch (Hashtbl.find(ctx.imports, name)) {
					case ::(p, []) if (=(p, pack)): [];
					case packs: if (!(List.mem(pack, packs))) {
							Hashtbl.replace(ctx.imports, name, ::(pack, packs));
						} else {
							[];
						};
					};
				} catch (e: Not_found) {
					Hashtbl.add(ctx.imports, name, ::(pack, []));
				};
				^ (String.concat("_", pack), ^ ("_", prefix_class(ctx.com, name)));
			};
		};
	};

	public static function s_path_haxe(path) return {
		switch ((new Tuple(fst(path), snd(path)))) {
		case ([], s): s;
		case (el, s): ^ (String.concat(".", el), ^ (".", s));
		};
	};

	public static function escape_bin(s) return {
		var b = Buffer.create(0);
		for (i in /*to*/0... - (String.length(s), 1)) {
			switch (Char.code(String.unsafe_get(s, i))) {
			case c if (||(=(c, Char.code('\\')), ||(=(c, Char.code('"')), =(c, Char.code('$'))))): Buffer.add_string(b, "\\");
				Buffer.add_char(b, Char.chr(c));
			case c if (<(c, 32)): Buffer.add_string(b, Printf.sprintf("\\x%.2X", c));
			case c: Buffer.add_char(b, Char.chr(c));
			};
		};
		Buffer.contents(b);
	};

	public static function is_keyword(n) return {
		switch (String.lowercase(n)) {
	case and | or | xor | __file__ | exception | __line__ | array | as | const | declare | die | echo | elseif | empty | enddeclare | endfor | endforeach | endif | endswitch | endwhile | eval | exit | foreach | global | include | include_once | isset | list | namespace | print | require | require_once | unset | use | __function__ | __class__ | __method__ | final | php_user_filter | protected | abstract | __set | __get | __call | clone | instanceof | break | case | class | continue | default | do | else | extends | for | function | if | new | return | static | switch | var | while | interface | implements | public | private | try | catch | throw | goto
																: True;
	case _: False;
	};
};

public static function s_ident(n) return {
		var suf = "h";
		if (is_keyword(n)) {
			^ (suf, n);
		} else {
			n;
		};
	};

	public static function s_ident_field(n) return {
		if (is_keyword(n)) {
			^ ("{\"", ^ (escape_bin(n), "\"}"));
		} else {
			n;
		};
	};

	public static function s_ident_local(n) return {
		var suf = "h";
		switch (String.lowercase(n)) {
		case globals | _server | _get | _post | _cookie | _files | _env | _request | _session: ^ (suf, n);
		case _: n;
		};
	};

	public static function create_directory(com, ldir) return {
		var atm_path = ref(String.create(0));
		atm_path.val = com.file;
		if (!(Sys.file_exists(com.file))) {
			Unix.mkdir(com.file, 0o755);
		} else {
			[];
		};
		List.iter(function p: atm_path.val = ^ (atm_path.val, ^ ("/", p));
		if (!(Sys.file_exists(atm_path.val))) {
		Unix.mkdir(atm_path.val, 0o755);
		} else {
			[];
		}, ldir);
	};

	public static function write_resource(dir, name, data) return {
		var rdir = ^ (dir, "/res");
		if (!(Sys.file_exists(dir))) {
			Unix.mkdir(dir, 0o755);
		} else {
			[];
		};
		if (!(Sys.file_exists(rdir))) {
			Unix.mkdir(rdir, 0o755);
		} else {
			[];
		};
		var name = Codegen.escape_res_name(name, False);
		var ch = open_out_bin( ^ (rdir, ^ ("/", name)));
		output_string(ch, data);
		close_out(ch);
	};

	public static function stack_init(com, use_add) return {
		Codegen.stack_context_init(com, "GLOBALS['%s']", "GLOBALS['%e']", "__hx__spos", "tmp", use_add, null_pos);
	};

	public static function init(com, cwd, path, def_type) return {
		function create(acc) return {
		case []: [];
		case ::(d, l): var pdir = String.concat("/", List.rev(::(d, acc)));
			if (!(Sys.file_exists(pdir))) {
				Unix.mkdir(pdir, 0o755);
			} else {
				[];
			};
			create(::(d, acc), l);
		};
		var dir = if (<>(cwd, "")) {
			::(com.file, ::(cwd, fst(path)));
		} else {
			::(com.file, fst(path));
		};
		create([], dir);
		function filename(path) return {
			prefix_class(com, switch (path) {
		case ([], List): "HList";
			case (_, s): s;
			});
		};
		var ch = open_out( ^ (String.concat("/", dir), ^ ("/", ^ (filename(path), ^ (if ( = (def_type, 0)) {
		".class";
	} else {
		if ( = (def_type, 1)) {
				".enum";
			} else {
				if ( = (def_type, 2)) {
					".interface";
				} else {
					".extern";
				};
			};
		}, ".php")))));
		var imports = Hashtbl.create(0);
		Hashtbl.add(imports, snd(path), ::(fst(path), []));
		var ctx = { () with com = com;
					stack = stack_init(com, False);
					tabs = "";
					ch = ch;
					path = path;
					buf = Buffer.create(lsl(1, 14));
					in_value = None;
					in_loop = False;
					in_instance_method = False;
					imports = imports;
					extern_required_paths = [];
					extern_classes_with_init = [];
					curclass = null_class;
					curmethod = "";
					locals = PMap.empty;
					inv_locals = PMap.empty;
					local_types = [];
					inits = [];
					constructor_block = False;
					dynamic_methods = [];
					all_dynamic_methods = [];
					is_call = False;
					cwd = cwd;
					inline_methods = [];
					nested_loops = 0;
					inline_index = 0;
					in_block = False;
		lib_path = switch (com.php_lib) {
	case None: "lib";
	case Some(s): s;
		}
				  };
		Codegen.map_source_header(com, function s: print(ctx, "// %s\n", s));
		ctx;
	};

	public static function unsupported(msg, p) return {
		error( ^ ("This expression cannot be generated to PHP: ", msg), p);
	};

	public static function newline(ctx) return {
		switch (Buffer.nth(ctx.buf, -(Buffer.length(ctx.buf), 1))) {
		case '{' | ':' | ' ' | '}' if (!=(Buffer.nth(ctx.buf, -(Buffer.length(ctx.buf), 2)), '"')): print(ctx, "\n%s", ctx.tabs);
		case _: print(ctx, ";\n%s", ctx.tabs);
		};
	};

	public static function concat(ctx, s, f) return {
	case []: [];
	case ::(x, []): f(x);
	case ::(x, l): f(x);
		spr(ctx, s);
		concat(ctx, s, f, l);
	};

	public static function open_block(ctx) return {
		var oldt = ctx.tabs;
		ctx.tabs = ^ ("\t", ctx.tabs);
		function []: ctx.tabs = oldt;
	};

	public static function parent(e) return {
		switch (e.eexpr) {
		case TParenthesis(_): e;
		case _: mk(TParenthesis(e), e.etype, e.epos);
		};
	};

	public static function inc_extern_path(ctx, path) return {
		function slashes(n) return {
			if ( = (n, 0)) {
				"";
			} else {
				^ ("../", slashes(-(n, 1)));
			};
		};
		var pre = if ( = (ctx.cwd, "")) {
			^ (ctx.lib_path, "/");
		} else {
			"";
		};
		switch (path) {
		case ([], name): ^ (pre, ^ (slashes(List.length(fst(ctx.path))), ^ (prefix_class(ctx.com, name), ".extern.php")));
		case (pack, name): ^ (pre, ^ (slashes(List.length(fst(ctx.path))), ^ (String.concat("/", pack), ^ ("/",
										  ^ (prefix_class(ctx.com, name), ".extern.php")))));
		};
	};

	public static function close(ctx) return {
		output_string(ctx.ch, "<?php\n");
		List.iter(function path:
		if (<>(path, ctx.path)) {
		output_string(ctx.ch, ^ ("require_once dirname[__FILE__].'/", ^ (inc_extern_path(ctx, path), "';\n")));
		} else {
			[];
		}, List.rev(ctx.extern_required_paths));
		output_string(ctx.ch, "\n");
		output_string(ctx.ch, Buffer.contents(ctx.buf));
		close_out(ctx.ch);
	};

	public static function save_locals(ctx) return {
		var old = ctx.locals;
		var old_inv = ctx.inv_locals;
		function []: ctx.locals = old;
		ctx.inv_locals = old_inv;
	};

	public static function define_local(ctx, l) return {
		function loop(n) return {
			var name = if ( = (n, 1)) {
				s_ident_local(l);
			} else {
				s_ident_local( ^ (l, string_of_int(n)));
			};
			if (PMap.mem(name, ctx.inv_locals)) {
				loop(+(n, 1));
			} else {
				ctx.locals = PMap.add(l, name, ctx.locals);
				ctx.inv_locals = PMap.add(name, l, ctx.inv_locals);
				name;
			};
		};
		loop(1);
	};

	public static function this(ctx) return {
		if (<>(ctx.in_value, None)) {
			"$__hx__this";
		} else {
			"$this";
		};
	};

	public static function gen_constant(ctx, p) return {
	case TInt(i): print(ctx, "%ld", i);
	case TFloat(s): spr(ctx, s);
	case TString(s): print(ctx, "\"%s\"", escape_bin(s));
	case TBool(b): spr(ctx, if (b) {
		"true";
	} else {
		"false";
	});
	case TNull: spr(ctx, "null");
	case TThis: spr(ctx, this(ctx));
	case TSuper: spr(ctx, "ERROR /* unexpected call to super in gen_constant */");
	};

	public static function arg_is_opt(c) return {
		switch (c) {
		case Some(_): True;
		case None: False;
		};
	};

	public static function s_funarg(ctx, arg, t, p, o) return {
		var byref = if ( && ( > (String.length(arg), 7), = (String.sub(arg, 0, 7), "byref__"))) {
			"&";
		} else {
			"";
		};
		print(ctx, "%s$%s", byref, s_ident_local(arg));
		if (o) {
			spr(ctx, " = null");
		} else {
			[];
		};
	};

	public static function is_in_dynamic_methods(ctx, e, s) return {
		List.exists(function dm: = ( ^ (String.concat(".", @(fst(dm.mpath), ::( ^ ("#", snd(dm.mpath)), []))), ^ (".", dm.mname)), ^ (s_type_name(e.etype), ^ (".", s))), ctx.all_dynamic_methods);
	};

	public static function is_dynamic_method(f) return {
		switch (f.cf_kind) {
		case Var(_): True;
		case Method(MethDynamic): True;
		case _: False;
		};
	};

	public static function fun_block(ctx, f, p) return {
		var e = switch (f.tf_expr) {
		case {
				eexpr = TBlock(::({ eexpr = TBlock(_) } = e, []))
			}: e;
		case e: e;
		};
		var e = List.fold_left(function e: function (v, c):
		switch (c) {
	case None | Some(TNull): e;
		case Some(c): Type.concat(Codegen.set_default(ctx.com, v, c, p), e);
		}, e, f.tf_args);
		if (ctx.com.debug) {
			Codegen.stack_block(ctx.stack, ctx.curclass, ctx.curmethod, e);
		} else {
			mk_block(e);
		};
	};

	public static function gen_array_args(ctx, lst) return {
		switch (lst) {
		case []: [];
		case ::(h, t): spr(ctx, "[");
			gen_value(ctx, h);
			spr(ctx, "]");
			gen_array_args(ctx, t);
		};
	};

	public static function gen_call(ctx, e, el) return {
		function genargs(lst) return {
			switch (lst) {
			case []: [];
			case ::(h, []): spr(ctx, " = ");
				gen_value(ctx, h);
			case ::(h, t): spr(ctx, "[");
				gen_value(ctx, h);
				spr(ctx, "]");
				genargs(t);
			};
		};
		switch ((new Tuple(e.eexpr, el))) {
		case (TConst(TSuper), params): switch (ctx.curclass.cl_super) {
			case None: assert False;
			case Some(c, _): spr(ctx, "parent::__construct[");
				concat(ctx, ",", gen_value(ctx), params);
				spr(ctx, "]");
			};
		case (TField({ eexpr = TConst(TSuper) }, f), params):
			switch (ctx.curclass.cl_super) {
			case None: assert False;
			case Some(c, _): print(ctx, "parent::%s[", s_ident(field_name(f)));
				concat(ctx, ",", gen_value(ctx), params);
				spr(ctx, "]");
			};
		case (TLocal({ v_name = __set__ }), ::({ eexpr = TConst(TString(code)) }, el)): print(ctx, "$%s", code);
			genargs(el);
		case (TLocal({ v_name = __set__ }), ::(e, el)): gen_value(ctx, e);
			genargs(el);
		case (TLocal({ v_name = __setfield__ }), ::(e, ::(f, el))): gen_value(ctx, e);
			spr(ctx, "->{");
			gen_value(ctx, f);
			spr(ctx, "}");
			genargs(el);
		case (TLocal({ v_name = __field__ }), ::(e, ::({ eexpr = TConst(TString(code)) }, el))): gen_value(ctx, e);
			spr(ctx, "->");
			spr(ctx, code);
			gen_array_args(ctx, el);
		case (TLocal({ v_name = __field__ }), ::(e, ::(f, el))): gen_value(ctx, e);
			spr(ctx, "->");
			gen_value(ctx, f);
			gen_array_args(ctx, el);
		case (TLocal({ v_name = __prefix__ }), []):
			switch (ctx.com.php_prefix) {
			case Some(prefix): print(ctx, "\"%s\"", prefix);
			case None: spr(ctx, "null");
			};
		case (TLocal({ v_name = __var__ }), ::({ eexpr = TConst(TString(code)) }, el)): print(ctx, "$%s", code);
			gen_array_args(ctx, el);
		case (TLocal({ v_name = __var__ }), ::(e, el)): gen_value(ctx, e);
			gen_array_args(ctx, el);
		case (TLocal({ v_name = __call__ }), ::({ eexpr = TConst(TString(code)) }, el)): spr(ctx, code);
			spr(ctx, "[");
			concat(ctx, ", ", gen_value(ctx), el);
			spr(ctx, "]");
		case (TLocal({ v_name = __php__ }), ::({ eexpr = TConst(TString(code)) }, [])): spr(ctx, prefix_init_replace(ctx.com,
					code));
		case (TLocal({ v_name = __php__ }), ::({ eexpr = TConst(TString(code)); epos = p }, tl)): Codegen.interpolate_code(ctx.com,
					code, tl, spr(ctx), gen_expr(ctx), p);
		case (TLocal({ v_name = __instanceof__ }), ::(e1, ::({ eexpr = TConst(TString(t)) }, []))): gen_value(ctx, e1);
			print(ctx, " instanceof %s", t);
		case (TLocal({ v_name = __physeq__ }), ::(e1, ::(e2, []))): spr(ctx, "[");
			gen_value(ctx, e1);
			spr(ctx, " === ");
			gen_value(ctx, e2);
			spr(ctx, "]");
		case (TLocal(_), []) | (TFunction(_), []) | (TCall(_), []) | (TParenthesis(_), []) | (TMeta(_), []) | (TBlock(_), []):
			ctx.is_call = True;
			spr(ctx, "call_user_func[");
			gen_value(ctx, e);
			ctx.is_call = False;
			spr(ctx, "]");
		case (TLocal(_), el) | (TFunction(_), el) | (TCall(_), el) | (TParenthesis(_), el) | (TMeta(_), el) | (TBlock(_), el):
			ctx.is_call = True;
			spr(ctx, "call_user_func_array[");
			gen_value(ctx, e);
			ctx.is_call = False;
			spr(ctx, ", array[");
			concat(ctx, ", ", gen_value(ctx), el);
			spr(ctx, "]]");
		case _: ctx.is_call = True;
			gen_value(ctx, e);
			ctx.is_call = False;
			spr(ctx, "[");
			concat(ctx, ", ", gen_value(ctx), el);
			spr(ctx, "]");
		};
	};

	public static function could_be_string_var(s) return {
		= (s, "length");
	};

	public static function gen_uncertain_string_var(ctx, s, e) return {
		switch (s) {
		case length: spr(ctx, "_hx_len[");
			gen_value(ctx, e);
			spr(ctx, "]");
		case _: gen_field_access(ctx, True, e, s);
		};
	};

	public static function gen_string_var(ctx, s, e) return {
		switch (s) {
		case length: spr(ctx, "strlen[");
			gen_value(ctx, e);
			spr(ctx, "]");
		case _: unsupported("gen_string_var ", e.epos);
		};
	};

	public static function gen_string_static_call(ctx, s, e, el) return {
		switch (s) {
		case fromCharCode: spr(ctx, "chr[");
			concat(ctx, ", ", gen_value(ctx), el);
			spr(ctx, "]");
		case _: unsupported("gen_string_static_call ", e.epos);
		};
	};

	public static function could_be_string_call(s) return {
		|| ( = (s, "substr"), || ( = (s, "substring"), || ( = (s, "charAt"), || ( = (s, "charCodeAt"), || ( = (s, "indexOf"), || ( = (s, "lastIndexOf"), || ( = (s, "split"), || ( = (s, "toLowerCase"), || ( = (s, "toString"), = (s, "toUpperCase"))))))))));
	};

	public static function gen_string_call(ctx, s, e, el) return {
		switch (s) {
		case substr: spr(ctx, "_hx_substr[");
			gen_value(ctx, e);
			spr(ctx, ", ");
			concat(ctx, ", ", gen_value(ctx), el);
			spr(ctx, "]");
		case substring: spr(ctx, "_hx_substring[");
			gen_value(ctx, e);
			spr(ctx, ", ");
			concat(ctx, ", ", gen_value(ctx), el);
			spr(ctx, "]");
		case charAt: spr(ctx, "_hx_char_at[");
			gen_value(ctx, e);
			spr(ctx, ", ");
			concat(ctx, ", ", gen_value(ctx), el);
			spr(ctx, "]");
		case cca: spr(ctx, "ord[substr[");
			gen_value(ctx, e);
			spr(ctx, ",");
			concat(ctx, ", ", gen_value(ctx), el);
			spr(ctx, ",1]]");
		case charCodeAt: spr(ctx, "_hx_char_code_at[");
			gen_value(ctx, e);
			spr(ctx, ", ");
			concat(ctx, ", ", gen_value(ctx), el);
			spr(ctx, "]");
		case indexOf: spr(ctx, "_hx_index_of[");
			gen_value(ctx, e);
			spr(ctx, ", ");
			concat(ctx, ", ", gen_value(ctx), el);
			spr(ctx, "]");
		case lastIndexOf: spr(ctx, "_hx_last_index_of[");
			gen_value(ctx, e);
			spr(ctx, ", ");
			concat(ctx, ", ", gen_value(ctx), el);
			spr(ctx, "]");
		case split: spr(ctx, "_hx_explode[");
			concat(ctx, ", ", gen_value(ctx), el);
			spr(ctx, ", ");
			gen_value(ctx, e);
			spr(ctx, "]");
		case toLowerCase: spr(ctx, "strtolower[");
			gen_value(ctx, e);
			spr(ctx, "]");
		case toUpperCase: spr(ctx, "strtoupper[");
			gen_value(ctx, e);
			spr(ctx, "]");
		case toString: gen_value(ctx, e);
		case _: unsupported("gen_string_call", e.epos);
		};
	};

	public static function gen_uncertain_string_call(ctx, s, e, el) return {
		spr(ctx, "_hx_string_call[");
		gen_value(ctx, e);
		print(ctx, ", \"%s\", array[", s);
		concat(ctx, ", ", gen_value(ctx), el);
		spr(ctx, "]]");
	};

	public static function gen_field_op(ctx, e) return {
		switch (e.eexpr) {
		case TField(f, s): switch (follow(e.etype)) {
			case TFun(_): gen_field_access(ctx, True, f, field_name(s));
			case _: gen_value_op(ctx, e);
			};
		case _: gen_value_op(ctx, e);
		};
	};

	public static function gen_value_op(ctx, e) return {
		switch (e.eexpr) {
		case TBinop(op, _, _) if (||(=(op, Ast.OpAnd), ||(=(op, Ast.OpOr), =(op, Ast.OpXor)))): gen_value(ctx, e);
		case _: gen_value(ctx, e);
		};
	};

	public static function is_static(t) return {
		switch (follow(t)) {
		case TAnon(a): switch (a.a_status.val) {
			case Statics(c): True;
			case _: False;
			};
		case _: False;
		};
	};

	public static function gen_member_access(ctx, isvar, e, s) return {
		switch (follow(e.etype)) {
		case TAnon(a): switch (a.a_status.val) {
			case EnumStatics(_): print(ctx, "::%s%s", if (isvar) {
				"$";
			} else {
				"";
			}, s_ident(s));
			case Statics(_): print(ctx, "::%s%s", if (isvar) {
				"$";
			} else {
				"";
			}, s_ident(s));
			case _: print(ctx, "->%s", if (isvar) {
				s_ident_field(s);
				} else {
					s_ident(s);
				});
			};
		case _: print(ctx, "->%s", if (isvar) {
			s_ident_field(s);
			} else {
				s_ident(s);
			});
		};
	};

	public static function gen_field_access(ctx, isvar, e, s) return {
		switch (e.eexpr) {
		case TTypeExpr(t): spr(ctx, s_path(ctx, t_path(t), False, e.epos));
			gen_member_access(ctx, isvar, e, s);
		case TLocal(_): gen_expr(ctx, e);
			print(ctx, "->%s", if (isvar) {
			s_ident_field(s);
			} else {
				s_ident(s);
			});
		case TArray(e1, e2): spr(ctx, "_hx_array_get[");
			gen_value(ctx, e1);
			spr(ctx, ", ");
			gen_value(ctx, e2);
			spr(ctx, "]");
			gen_member_access(ctx, isvar, e, s);
		case TBlock(_) | TParenthesis(_) | TMeta(_) | TObjectDecl(_) | TArrayDecl(_) | TNew(_): spr(ctx, "_hx_deref[");
			ctx.is_call = False;
			gen_value(ctx, e);
			spr(ctx, "]");
			gen_member_access(ctx, isvar, e, s);
		case TCast(ec, _) if (switch (ec.eexpr) {
				case TNew(_) | TArrayDecl(_): True;
					case _: False;
					}): spr(ctx, "_hx_deref[");
			ctx.is_call = False;
			gen_value(ctx, e);
			spr(ctx, "]");
			gen_member_access(ctx, isvar, e, s);
		case _: gen_expr(ctx, e);
			gen_member_access(ctx, isvar, e, s);
		};
	};

	public static function gen_dynamic_function(ctx, isstatic, name, f, params, p) return {
		var old = ctx.in_value;
		var old_l = ctx.locals;
		var old_li = ctx.inv_locals;
		var old_t = ctx.local_types;
		ctx.in_value = None;
		ctx.local_types = @(List.map(snd, params), ctx.local_types);
		var byref = if ( && ( > (String.length(name), 9), = (String.sub(name, 0, 9), "__byref__"))) {
			"&";
		} else {
			"";
		};
		print(ctx, "function %s%s[", byref, name);
		concat(ctx, ", ", function (v, c): var arg = define_local(ctx, v.v_name);
			   s_funarg(ctx, arg, v.v_type, p, arg_is_opt(c)), f.tf_args);
		spr(ctx, "] {");
		if ( > (List.length(f.tf_args), 0)) {
			if (isstatic) {
				print(ctx, " return call_user_func_array[self::$%s, array[", name);
			} else {
				print(ctx, " return call_user_func_array[$this->%s, array[", name);
			};
			concat(ctx, ", ", function (v, _): spr(ctx, ^ ("$", v.v_name)), f.tf_args);
			print(ctx, "]]; }");
		} else {
			if (isstatic) {
				print(ctx, " return call_user_func[self::$%s]; }", name);
			} else {
				print(ctx, " return call_user_func[$this->%s]; }", name);
			};
		};
		newline(ctx);
		if (isstatic) {
			print(ctx, "public static $%s = null", name);
		} else {
			print(ctx, "public $%s = null", name);
		};
		ctx.in_value = old;
		ctx.locals = old_l;
		ctx.inv_locals = old_li;
		ctx.local_types = old_t;
	};

	public static function gen_function(ctx, name, f, params, p) return {
		var old = ctx.in_value;
		var old_l = ctx.locals;
		var old_li = ctx.inv_locals;
		var old_t = ctx.local_types;
		ctx.in_value = None;
		ctx.local_types = @(List.map(snd, params), ctx.local_types);
		var byref = if ( && ( > (String.length(name), 9), = (String.sub(name, 0, 9), "__byref__"))) {
			"&";
		} else {
			"";
		};
		print(ctx, "function %s%s[", byref, name);
		concat(ctx, ", ", function (v, o): var arg = define_local(ctx, v.v_name);
			   s_funarg(ctx, arg, v.v_type, p, arg_is_opt(o)), f.tf_args);
		print(ctx, "] ");
		gen_expr(ctx, fun_block(ctx, f, p));
		ctx.in_value = old;
		ctx.locals = old_l;
		ctx.inv_locals = old_li;
		ctx.local_types = old_t;
	};

	public static function gen_inline_function(ctx, f, hasthis, p) return {
		ctx.nested_loops = -(ctx.nested_loops, 1);
		var old = ctx.in_value;
		var old_l = ctx.locals;
		var old_li = ctx.inv_locals;
		var old_t = ctx.local_types;
		ctx.in_value = Some("closure");
		function args(a) return {
			List.map(function (v, _): v.v_name, a);
		};
		var used_locals = ref(PMap.empty);
		function loop(e) return {
			switch (e.eexpr) {
			case TLocal(v) if (&&(!(start_with(v.v_name, "__hx__")), PMap.mem(v.v_name, old_l))): used_locals.val = PMap.add(v.v_name,
				v.v_name, used_locals.val);
			case _: Type.iter(loop, e);
			};
		};
		loop(f.tf_expr);
		spr(ctx, "array[new _hx_lambda[array[");
		var c = ref(0);
		function print_arg(a) return {
			if ( > (c.val, 0)) {
				spr(ctx, ", ");
			} else {
				[];
			};
			incr(c);
			print(ctx, "&$%s", a);
		};
		if (hasthis) {
			print_arg("this");
		} else {
			[];
		};
		PMap.iter(function _: function a: print_arg(a), used_locals.val);
		spr(ctx, "], \"");
		spr(ctx, inline_function(ctx, args(f.tf_args), hasthis, used_locals.val, fun_block(ctx, f, p)));
		print(ctx, "\"], 'execute']");
		ctx.in_value = old;
		ctx.locals = old_l;
		ctx.inv_locals = old_li;
		ctx.local_types = old_t;
		ctx.nested_loops = +(ctx.nested_loops, 1);
	};

	public static function unset_locals(ctx, old_l) return {
		var lst = ref([]);
		PMap.iter(function n: function _:
		if (!(PMap.exists(n, old_l))) {
		lst.val = @(::( ^ ("$", n), []), lst.val);
		} else {
			[];
		}, ctx.inv_locals);
		if ( > (List.length(lst.val), 0)) {
			newline(ctx);
			spr(ctx, "unset[");
			concat(ctx, ",", function s: spr(ctx, s), lst.val);
			spr(ctx, "]");
		} else {
			[];
		};
	};

	public static function gen_while_expr(ctx, e) return {
		var old_loop = ctx.in_loop;
		ctx.in_loop = True;
		var old_nested_loops = ctx.nested_loops;
		ctx.nested_loops = 1;
		var old_l = ctx.inv_locals;
		var b = save_locals(ctx);
		switch (e.eexpr) {
		case TBlock(el): List.iter(function e: newline(ctx);
			gen_expr(ctx, e), el);
		case _: newline(ctx);
			gen_expr(ctx, e);
		};
		unset_locals(ctx, old_l);
		b([]);
		ctx.nested_loops = old_nested_loops;
		ctx.in_loop = old_loop;
	};

	public static function gen_tfield(ctx, e, e1, s) return {
		switch (follow(e.etype)) {
		case TFun(args, _): if (ctx.is_call) {
				gen_field_access(ctx, False, e1, s);
			} else {
				if (is_in_dynamic_methods(ctx, e1, s)) {
					gen_field_access(ctx, True, e1, s);
				} else {
					function ob(ex) return {
						switch (ex) {
						case TTypeExpr(t): print(ctx, "\"");
							spr(ctx, s_path(ctx, t_path(t), False, e1.epos));
							print(ctx, "\"");
						case _: gen_expr(ctx, e1);
						};
					};
					spr(ctx, "[isset[");
					gen_field_access(ctx, True, e1, s);
					spr(ctx, "] ? ");
					gen_field_access(ctx, True, e1, s);
					spr(ctx, ": array[");
					ob(e1.eexpr);
					print(ctx, ", \"%s\"]]", s_ident(s));
				};
			};
		case TMono(_): if (ctx.is_call) {
				gen_field_access(ctx, False, e1, s);
			} else {
				gen_uncertain_string_var(ctx, s, e1);
			};
		case _: if (is_string_expr(e1)) {
				gen_string_var(ctx, s, e1);
			} else {
				if (is_uncertain_expr(e1)) {
					gen_uncertain_string_var(ctx, s, e1);
				} else {
					gen_field_access(ctx, True, e1, s);
				};
			};
		};
	};

	public static function gen_expr(ctx, e) return {
		var in_block = ctx.in_block;
		ctx.in_block = False;
		function restore_in_block(ctx, inb) return {
			if (inb) {
				ctx.in_block = True;
			} else {
				[];
			};
		};
		switch (e.eexpr) {
		case TConst(c): gen_constant(ctx, e.epos, c);
		case TLocal(v): spr(ctx, ^ ("$", try {
				PMap.find(v.v_name, ctx.locals);
			} catch (e: Not_found) {
				s_ident_local(v.v_name);
			}));
		case TArray(e1, e2): switch (e1.eexpr) {
			case TCall(_) | TBlock(_) | TParenthesis(_) | TMeta(_) | TArrayDecl(_): spr(ctx, "_hx_array_get[");
				gen_value(ctx, e1);
				spr(ctx, ", ");
				gen_value(ctx, e2);
				spr(ctx, "]");
			case TCast(ec, _) if (switch (ec.eexpr) {
					case TArrayDecl(_) | TBlock(_): True;
						case _: False;
						}): spr(ctx, "_hx_array_get[");
				gen_value(ctx, e1);
				spr(ctx, ", ");
				gen_value(ctx, e2);
				spr(ctx, "]");
			case _: gen_value(ctx, e1);
				spr(ctx, "[");
				gen_value(ctx, e2);
				spr(ctx, "]");
			};
		case TBinop(op, e1, e2): function non_assoc(match) return switch (match) {
			case Ast.OpEq | Ast.OpNotEq | Ast.OpGt | Ast.OpGte | Ast.OpLt | Ast.OpLte: True;
			case _: False;
			};
			switch (e1.eexpr) {
			case TBinop(op2, _, _) if (&&(non_assoc(op), non_assoc(op2))): gen_expr(ctx, { (e) with eexpr = TBinop(op, mk(TParenthesis(e1), e1.etype, e1.epos), e2) });
			case _: 	function leftside(e) return {
					switch (e.eexpr) {
					case TArray(te1, te2): gen_value(ctx, te1);
						spr(ctx, "->a[");
						gen_value(ctx, te2);
						spr(ctx, "]");
					case _: gen_field_op(ctx, e1);
					};
				};
				function leftsidec(e) return {
					switch (e.eexpr) {
					case TArray(te1, te2): gen_value(ctx, te1);
						spr(ctx, "->a[");
						gen_value(ctx, te2);
						spr(ctx, "]");
					case TField(e1, s): gen_field_access(ctx, True, e1, field_name(s));
					case _: gen_field_op(ctx, e1);
					};
				};
				function leftsidef(e) return {
					switch (e.eexpr) {
					case TField(e1, s): gen_field_access(ctx, True, e1, field_name(s));
					case _: gen_field_op(ctx, e1);
					};
				};
				switch (op) {
				case Ast.OpMod: spr(ctx, "_hx_mod[");
					gen_value_op(ctx, e1);
					spr(ctx, ", ");
					gen_value_op(ctx, e2);
					spr(ctx, "]");
				case Ast.OpAssign: switch (e1.eexpr) {
					case TArray(te1, te2) if (switch (te1.eexpr) {
							case TCall(_) | TParenthesis(_): True;
								case _: False;
								}): spr(ctx, "_hx_array_assign[");
						gen_value(ctx, te1);
						spr(ctx, ", ");
						gen_value(ctx, te2);
						spr(ctx, ", ");
						gen_value_op(ctx, e2);
						spr(ctx, "]");
					case _: leftsidef(e1);
						spr(ctx, " = ");
						gen_value_op(ctx, e2);
					};
				case Ast.OpAssignOp(Ast.OpAdd) if (&&(is_uncertain_expr(e1), is_uncertain_expr(e2))):
					switch (e1.eexpr) {
					case TArray(te1, te2): var t1 = define_local(ctx, "__hx__t1");
						var t2 = define_local(ctx, "__hx__t2");
						print(ctx, "_hx_array_assign[$%s = ", t1);
						gen_value(ctx, te1);
						print(ctx, ", $%s = ", t2);
						gen_value(ctx, te2);
						print(ctx, ", $%s->a[$%s] + ", t1, t2);
						gen_value_op(ctx, e2);
						spr(ctx, "]");
					case _: leftside(e1);
						spr(ctx, " = ");
						spr(ctx, "_hx_add[");
						gen_value_op(ctx, e1);
						spr(ctx, ", ");
						gen_value_op(ctx, e2);
						spr(ctx, "]");
					};
				case Ast.OpAssignOp(Ast.OpAdd) if (||(is_string_expr(e1), is_string_expr(e2))): leftside(e1);
					spr(ctx, " .= ");
					gen_value_op(ctx, as_string_expr(ctx, e2));
				case Ast.OpAssignOp(Ast.OpShl): leftside(e1);
					spr(ctx, " < <= ");
					gen_value_op(ctx, e2);
				case Ast.OpAssignOp(Ast.OpUShr): leftside(e1);
					spr(ctx, " = ");
					spr(ctx, "_hx_shift_right[");
					gen_value_op(ctx, e1);
					spr(ctx, ", ");
					gen_value_op(ctx, e2);
					spr(ctx, "]");
				case Ast.OpAssignOp(Ast.OpMod): leftside(e1);
					spr(ctx, " = ");
					spr(ctx, "_hx_mod[");
					gen_value_op(ctx, e1);
					spr(ctx, ", ");
					gen_value_op(ctx, e2);
					spr(ctx, "]");
				case Ast.OpAssignOp(_): leftsidec(e1);
					print(ctx, " %s ", Ast.s_binop(op));
					gen_value_op(ctx, e2);
				case Ast.OpAdd if (&&(is_uncertain_expr(e1), is_uncertain_expr(e2))): spr(ctx, "_hx_add[");
					gen_value_op(ctx, e1);
					spr(ctx, ", ");
					gen_value_op(ctx, e2);
					spr(ctx, "]");
				case Ast.OpAdd if (||(is_string_expr(e1), is_string_expr(e2))): gen_value_op(ctx, as_string_expr(ctx, e1));
					spr(ctx, " . ");
					gen_value_op(ctx, as_string_expr(ctx, e2));
				case Ast.OpShl: gen_value_op(ctx, e1);
					spr(ctx, " < < ");
					gen_value_op(ctx, e2);
				case Ast.OpUShr: spr(ctx, "_hx_shift_right[");
					gen_value_op(ctx, e1);
					spr(ctx, ", ");
					gen_value_op(ctx, e2);
					spr(ctx, "]");
				case Ast.OpNotEq | Ast.OpEq: var s_op = if ( = (op, Ast.OpNotEq)) {
						" != ";
					} else {
						" == ";
					};
					var s_phop = if ( = (op, Ast.OpNotEq)) {
						" !== ";
					} else {
						" === ";
					};
					var se1 = s_expr_name(e1);
					var se2 = s_expr_name(e2);
					if ( || ( = (e1.eexpr, TConst(TNull)), = (e2.eexpr, TConst(TNull)))) {
						switch (e1.eexpr) {
						case TField(f, s) if (||(is_anonym_expr(e1), is_unknown_expr(e1))): spr(ctx, "_hx_field[");
							gen_value(ctx, f);
							print(ctx, ", \"%s\"]", field_name(s));
						case _: gen_field_op(ctx, e1);
						};
						spr(ctx, s_phop);
						switch (e2.eexpr) {
						case TField(f, s) if (||(is_anonym_expr(e2), is_unknown_expr(e2))): spr(ctx, "_hx_field[");
							gen_value(ctx, f);
							print(ctx, ", \"%s\"]", field_name(s));
						case _: gen_field_op(ctx, e2);
						};
					} else {
						if ( || ( && ( || ( = (se1, "Int"), = (se1, "Null<Int>")), || ( = (se2, "Int"), = (se2, "Null<Int>"))), && (
									  || ( = (se1, "Float"), = (se1, "Null<Float>")), || ( = (se2, "Float"), = (se2, "Null<Float>"))))) {
							gen_field_op(ctx, e1);
							spr(ctx, s_phop);
							gen_field_op(ctx, e2);
						} else {
							if ( || ( && ( || ( = (se1, "Int"), || ( = (se1, "Float"), || ( = (se1, "Null<Int>"), = (se1, "Null<Float>")))),
										   || ( = (se1, "Int"), || ( = (se1, "Float"), || ( = (se1, "Null<Int>"), = (se1, "Null<Float>"))))), || (
										  && (is_unknown_expr(e1), is_unknown_expr(e2)), || (is_anonym_expr(e1), is_anonym_expr(e2))))) {
								if ( = (op, Ast.OpNotEq)) {
									spr(ctx, "!");
								} else {
									[];
								};
								spr(ctx, "_hx_equal[");
								gen_field_op(ctx, e1);
								spr(ctx, ", ");
								gen_field_op(ctx, e2);
								spr(ctx, "]");
							} else {
								if ( && ( || ( == (se1, se2), || (switch (e1.eexpr) {
								case TConst(_) | TLocal(_) | TArray(_) | TNew(_): True;
									case _: False;
									}, || (switch (e2.eexpr) {
							case TConst(_) | TLocal(_) | TArray(_) | TNew(_): True;
								case _: False;
								}, || (is_string_expr(e1), || (is_string_expr(e2), || (is_anonym_expr(e1), || (is_anonym_expr(e2),
															   || (is_unknown_expr(e1), is_unknown_expr(e2))))))))),
								&& (<>(type_string(follow(e1.etype)), "Dynamic"), <>(type_string(follow(e2.etype)), "Dynamic")))) {
									gen_field_op(ctx, e1);
									spr(ctx, s_phop);
									gen_field_op(ctx, e2);
								} else {
									var tmp = define_local(ctx, "_t");
									print(ctx, "[is_object[$%s = ", tmp);
									gen_field_op(ctx, e1);
									print(ctx, "] && ![$%s instanceof Enum] ? $%s%s", tmp, tmp, s_phop);
									gen_field_op(ctx, e2);
									print(ctx, " : $%s%s", tmp, s_op);
									gen_field_op(ctx, e2);
									spr(ctx, "]");
								};
							};
						};
					};
				case Ast.OpGt | Ast.OpGte | Ast.OpLt | Ast.OpLte if (is_string_expr(e1)): spr(ctx, "[strcmp[");
					gen_field_op(ctx, e1);
					spr(ctx, ", ");
					gen_field_op(ctx, e2);
					spr(ctx, "]");
					var op_str = switch (op) {
					case Ast.OpGt: ">";
					case Ast.OpGte: ">=";
					case Ast.OpLt: "<";
					case Ast.OpLte: "<=";
					case _: assert False;
					};
					print(ctx, "%s 0]", op_str);
				case _: leftside(e1);
					print(ctx, " %s ", Ast.s_binop(op));
					gen_value_op(ctx, e2);
				};
			};
		case TEnumParameter(e1, _, i): spr(ctx, "_hx_deref[");
			gen_value(ctx, e1);
			spr(ctx, "]");
			print(ctx, "->params[%d]", i);
		case TField(e1, s): gen_tfield(ctx, e, e1, field_name(s));
		case TTypeExpr(t): print(ctx, "_hx_qtype[\"%s\"]", s_path_haxe(t_path(t)));
		case TParenthesis(e): switch (e.eexpr) {
			case TParenthesis(_) | TReturn(_): gen_value(ctx, e);
			case _: spr(ctx, "[");
				gen_value(ctx, e);
				spr(ctx, "]");
			};
		case TMeta(_, e): gen_expr(ctx, e);
		case TReturn(eo): switch (eo) {
			case None: spr(ctx, "return");
			case Some(e) if (switch (follow(e.etype)) {
					case TEnum({ e_path = ([], Void) }, []) | TAbstract({ a_path = ([], Void) }, []): True;
						case _: False;
						}): gen_value(ctx, e);
				newline(ctx);
				spr(ctx, "return");
			case Some(e): switch (e.eexpr) {
				case TThrow(_): [];
				case _: spr(ctx, "return ");
				};
				gen_value(ctx, e);
			};
		case TBreak: if (ctx.in_loop) {
				spr(ctx, "break");
			} else {
				print(ctx, "break %d", ctx.nested_loops);
			};
		case TContinue: if (ctx.in_loop) {
				spr(ctx, "continue");
			} else {
				print(ctx, "continue %d", ctx.nested_loops);
			};
		case TBlock([]): spr(ctx, "{}");
		case TBlock(el): var old_l = ctx.inv_locals;
			var b = save_locals(ctx);
			print(ctx, "{");
			var bend = open_block(ctx);
			var cb = if (!(ctx.constructor_block)) {
				function []: [];
			} else {
				ctx.constructor_block = False;
				if ( > (List.length(ctx.dynamic_methods), 0)) {
					newline(ctx);
				} else {
					spr(ctx, " ");
				};
				List.iter(function f: var name = f.cf_name;
				switch (f.cf_expr) {
			case Some({ eexpr = TFunction(fd) }): print(ctx, "if[!isset[$this->%s]] $this->%s = ", name, name);
					gen_inline_function(ctx, fd, True, e.epos);
					newline(ctx);
				case _: [];
				}, ctx.dynamic_methods);
				if (Codegen.constructor_side_effects(e)) {
					print(ctx, "if[!%s::$skip_constructor] {", s_path(ctx, (new Tuple(::("php", []), "Boot")), False, e.epos));
					function []: print(ctx, "}");
				} else {
					function []: [];
				};
			};
			var remaining = ref(List.length(el));
			function build(e) return {
				newline(ctx);
				if ( && (in_block, = (remaining.val, 1))) {
					switch (e.eexpr) {
					case TIf(_) | TSwitch(_) | TThrow(_) | TWhile(_) | TFor(_) | TTry(_) | TBreak | TBlock(_): restore_in_block(ctx, in_block);
						gen_expr(ctx, e);
						unset_locals(ctx, old_l);
					case TReturn(Some(e1)): switch (e1.eexpr) {
						case TIf(_) | TSwitch(_) | TThrow(_) | TWhile(_) | TFor(_) | TTry(_) | TBlock(_): [];
						case _: spr(ctx, "return ");
						};
						gen_expr(ctx, e1);
					case _: spr(ctx, "return ");
						gen_value(ctx, e);
					};
				} else {
					gen_expr(ctx, e);
				};
				decr(remaining);
			};
			List.iter(build, el);
			if (ctx.in_loop) {
				unset_locals(ctx, old_l);
			} else {
				[];
			};
			bend([]);
			newline(ctx);
			cb([]);
			print(ctx, "}");
			b([]);
		case TFunction(f): var old = (new Tuple(ctx.in_value, ctx.in_loop));
			var old_meth = ctx.curmethod;
			ctx.in_value = None;
			ctx.in_loop = False;
			ctx.curmethod = ^ (ctx.curmethod, ^ ("@", string_of_int(Lexer.get_error_line(e.epos))));
			gen_inline_function(ctx, f, False, e.epos);
			ctx.curmethod = old_meth;
			ctx.in_value = fst(old);
			ctx.in_loop = snd(old);
		case TCall(ec, el): switch (ec.eexpr) {
			case TArray(_): spr(ctx, "call_user_func_array[");
				gen_value(ctx, ec);
				spr(ctx, ", array[");
				concat(ctx, ", ", gen_value(ctx), el);
				spr(ctx, "]]");
			case TField(ef, s) if (&&(is_static(ef.etype), is_string_expr(ef))): gen_string_static_call(ctx, field_name(s), ef, el);
			case TField(ef, s) if (is_string_expr(ef)): gen_string_call(ctx, field_name(s), ef, el);
			case TField(ef, s) if (&&(is_anonym_expr(ef), could_be_string_call(field_name(s)))): gen_uncertain_string_call(ctx,
						field_name(s), ef, el);
			case _: gen_call(ctx, ec, el);
			};
		case TArrayDecl(el): spr(ctx, "[new _hx_array[array[");
			concat(ctx, ", ", gen_value(ctx), el);
			spr(ctx, "]]]");
		case TThrow(e): spr(ctx, "throw new HException[");
			gen_value(ctx, e);
			spr(ctx, "]");
		case TVar(v, eo): spr(ctx, "$");
			var restore = save_locals(ctx);
			var n = define_local(ctx, v.v_name);
			var restore2 = save_locals(ctx);
			restore([]);
			switch (eo) {
			case None: print(ctx, "%s = null", s_ident_local(n));
			case Some(e): print(ctx, "%s = ", s_ident_local(n));
				gen_value(ctx, e);
			};
			restore2([]);
		case TNew(c, _, el): switch ((new Tuple(c.cl_path, el))) {
			case (([], String), _): concat(ctx, "", gen_value(ctx), el);
			case (([], Array), el): spr(ctx, "new _hx_array[array[");
				concat(ctx, ", ", gen_value(ctx), el);
				spr(ctx, "]]");
			case ((_, _), _): print(ctx, "new %s[", s_path(ctx, c.cl_path, c.cl_extern, e.epos));
				var count = ref(-1);
				concat(ctx, ", ", function e: incr(count);
				switch (c.cl_constructor) {
			case Some(f): gen_value(ctx, e);
				case _: [];
				}, el);
				spr(ctx, "]");
			};
		case TIf(cond, e, eelse): spr(ctx, "if");
			gen_value(ctx, parent(cond));
			spr(ctx, " ");
			restore_in_block(ctx, in_block);
			gen_expr(ctx, mk_block(e));
			switch (eelse) {
			case None: [];
			case Some(e) if (=(e.eexpr, TConst(TNull))): [];
			case Some(e): spr(ctx, " else ");
				restore_in_block(ctx, in_block);
				gen_expr(ctx, mk_block(e));
			};
		case TUnop(op, Ast.Prefix, e): switch (e.eexpr) {
			case TArray(te1, te2): switch (op) {
				case Increment: spr(ctx, "_hx_array_increment[");
					gen_value(ctx, te1);
					spr(ctx, ",");
					gen_value(ctx, te2);
					spr(ctx, "]");
				case Decrement: spr(ctx, "_hx_array_decrement[");
					gen_value(ctx, te1);
					spr(ctx, ",");
					gen_value(ctx, te2);
					spr(ctx, "]");
				case _: spr(ctx, Ast.s_unop(op));
					gen_value(ctx, te1);
					spr(ctx, "[");
					gen_value(ctx, te2);
					spr(ctx, "]");
				};
			case TField(e1, s): spr(ctx, Ast.s_unop(op));
				gen_tfield(ctx, e, e1, field_name(s));
			case _: spr(ctx, Ast.s_unop(op));
				gen_value(ctx, e);
			};
		case TUnop(op, Ast.Postfix, e): switch (e.eexpr) {
			case TArray(te1, te2): gen_value(ctx, te1);
				spr(ctx, "->a[");
				gen_value(ctx, te2);
				spr(ctx, "]");
			case TField(e1, s): gen_field_access(ctx, True, e1, field_name(s));
			case _: gen_value(ctx, e);
			};
			spr(ctx, Ast.s_unop(op));
		case TWhile(cond, e, Ast.NormalWhile): var old = save_locals(ctx);
			spr(ctx, "while");
			gen_value(ctx, parent(cond));
			spr(ctx, " {");
			var bend = open_block(ctx);
			gen_while_expr(ctx, e);
			bend([]);
			newline(ctx);
			spr(ctx, "}");
			old([]);
		case TWhile(cond, e, Ast.DoWhile): var old = save_locals(ctx);
			spr(ctx, "do {");
			var bend = open_block(ctx);
			gen_while_expr(ctx, e);
			bend([]);
			newline(ctx);
			spr(ctx, "} while");
			gen_value(ctx, parent(cond));
			old([]);
		case TObjectDecl(fields): spr(ctx, "_hx_anonymous[array[");
			concat(ctx, ", ", function (f, e): print(ctx, "\"%s\" => ", escape_bin(f));
				   gen_value(ctx, e), fields);
			spr(ctx, "]]");
		case TFor(v, it, e): var b = save_locals(ctx);
			var tmp = define_local(ctx, "__hx__it");
			var v = define_local(ctx, v.v_name);
			switch (it.eexpr) {
			case TCall(e, _): switch (e.eexpr) {
				case TField(e, f): spr(ctx, "if[null == ");
					gen_value(ctx, e);
					spr(ctx, "] throw new HException['null iterable']");
					newline(ctx);
				case _: [];
				};
			case _: [];
			};
			print(ctx, "$%s = ", tmp);
			gen_value(ctx, it);
			newline(ctx);
			print(ctx, "while[$%s->hasNext[]] {", tmp);
			var bend = open_block(ctx);
			newline(ctx);
			print(ctx, "unset[$%s]", v);
			newline(ctx);
			print(ctx, "$%s = $%s->next[]", v, tmp);
			gen_while_expr(ctx, e);
			bend([]);
			newline(ctx);
			spr(ctx, "}");
			b([]);
		case TTry(e, catchs): spr(ctx, "try ");
			restore_in_block(ctx, in_block);
			gen_expr(ctx, mk_block(e));
			var old = save_locals(ctx);
			var ex = define_local(ctx, "__hx__e");
			print(ctx, "catch[Exception $%s] {", ex);
			var bend = open_block(ctx);
			var first = ref(True);
			var catchall = ref(False);
			var evar = define_local(ctx, "_ex_");
			newline(ctx);
			print(ctx, "$%s = [$%s instanceof HException] ? $%s->e : $%s", evar, ex, ex, ex);
			old([]);
			List.iter(function (v, e): var ev = define_local(ctx, v.v_name);
					  newline(ctx);
					  var b = save_locals(ctx);
			if (!(first.val)) {
			spr(ctx, "else ");
			} else {
				[];
			};
			switch (follow(v.v_type)) {
		case TEnum(te, _): switch (te.e_path) {
				case ([], Bool): print(ctx, "if[is_bool[$%s = $%s]]", ev, evar);
				case _: print(ctx, "if[[$%s = $%s] instanceof %s]", ev, evar, s_path(ctx, te.e_path, te.e_extern, e.epos));
				};
				restore_in_block(ctx, in_block);
				gen_expr(ctx, mk_block(e));
			case TInst(tc, _): switch (tc.cl_path) {
				case ([], Int): print(ctx, "if[is_int[$%s = $%s]]", ev, evar);
				case ([], Float): print(ctx, "if[is_numeric[$%s = $%s]]", ev, evar);
				case ([], String): print(ctx, "if[is_string[$%s = $%s]]", ev, evar);
				case ([], Array): print(ctx, "if[[$%s = $%s] instanceof _hx_array]", ev, evar);
				case _: print(ctx, "if[[$%s = $%s] instanceof %s]", ev, evar, s_path(ctx, tc.cl_path, tc.cl_extern, e.epos));
				};
				restore_in_block(ctx, in_block);
				gen_expr(ctx, mk_block(e));
			case TAbstract(ta, _): switch (ta.a_path) {
				case ([], Int): print(ctx, "if[is_int[$%s = $%s]]", ev, evar);
				case ([], Float): print(ctx, "if[is_numeric[$%s = $%s]]", ev, evar);
				case ([], Bool): print(ctx, "if[is_bool[$%s = $%s]]", ev, evar);
				case _: print(ctx, "if[[$%s = $%s] instanceof %s]", ev, evar, s_path(ctx, ta.a_path, False, e.epos));
				};
				restore_in_block(ctx, in_block);
				gen_expr(ctx, mk_block(e));
			case TFun(_) | TLazy(_) | TType(_) | TAnon(_): assert False;
			case TMono(_) | TDynamic(_): catchall.val = True;
				if (!(first.val)) {
					spr(ctx, "{ ");
				} else {
					[];
				};
				print(ctx, "$%s = $%s", ev, evar);
				newline(ctx);
				restore_in_block(ctx, in_block);
				gen_expr(ctx, mk_block(e));
				if (!(first.val)) {
					spr(ctx, "}");
				} else {
					[];
				};
			};
			b([]);
			first.val = False, catchs);
			if (!(catchall.val)) {
				print(ctx, " else throw $%s;", ex);
			} else {
				[];
			};
			bend([]);
			newline(ctx);
			spr(ctx, "}");
		case TSwitch(e, cases, def): var old_loop = ctx.in_loop;
			ctx.in_loop = False;
			ctx.nested_loops = +(ctx.nested_loops, 1);
			var old = save_locals(ctx);
			spr(ctx, "switch");
			gen_value(ctx, parent(e));
			spr(ctx, " {");
			newline(ctx);
			List.iter(function (el, e2): List.iter(function e: spr(ctx, "case ");
												   gen_value(ctx, e);
												   spr(ctx, ":"), el);
					  restore_in_block(ctx, in_block);
					  gen_expr(ctx, mk_block(e2));
					  print(ctx, "break");
					  newline(ctx), cases);
			switch (def) {
			case None: [];
			case Some(e): spr(ctx, "default:");
				restore_in_block(ctx, in_block);
				gen_expr(ctx, mk_block(e));
				print(ctx, "break");
				newline(ctx);
			};
			spr(ctx, "}");
			ctx.nested_loops = -(ctx.nested_loops, 1);
			ctx.in_loop = old_loop;
			old([]);
		case TCast(e, None): gen_expr(ctx, e);
		case TCast(e1, Some(t)): function mk_texpr(match) return switch (match) {
			case TClassDecl(c): TAnon({ () with a_fields = PMap.empty;
											a_status = ref(Statics(c))
										  });
			case TEnumDecl(e): TAnon({ () with a_fields = PMap.empty;
										   a_status = ref(EnumStatics(e))
										 });
			case TAbstractDecl(a): TAnon({ () with a_fields = PMap.empty;
											   a_status = ref(AbstractStatics(a))
											 });
			case TTypeDecl(_): assert False;
			};
			spr(ctx, "_hx_cast[");
			gen_expr(ctx, e1);
			spr(ctx, ", ");
			gen_expr(ctx, mk(TTypeExpr(t), mk_texpr(t), e1.epos));
			spr(ctx, "]");
		};
	};

	public static function argument_list_from_locals(include_this, in_var, l) return {
		var lst = ref([]);
		if ( && (include_this, in_var)) {
			lst.val = ::("__hx__this", lst.val);
		} else {
			if (include_this) {
				lst.val = ::("this", lst.val);
			} else {
				[];
			};
		};
		PMap.iter(function n: function _: lst.val = @(lst.val, ::(n, [])), l);
		lst.val;
	};

	public static function remove_internals(args) return {
		List.filter(function a: || ( = (a, "__hx__this"), !(start_with(a, "__hx__"))), args);
	};

	public static function inline_block(ctx, e) return {
		var index = ctx.inline_index;
		ctx.inline_index = +(ctx.inline_index, 1);
		var block = {
			() with iname = ^ (s_path(ctx, ctx.curclass.cl_path, ctx.curclass.cl_extern, ctx.curclass.cl_pos), ^ ("_", string_of_int(index)));
			iindex = index;
			ihasthis = ctx.in_instance_method;
			iarguments = [];
			iexpr = e;
			ilocals = ctx.locals;
			iin_block = True;
			iinv_locals = ctx.inv_locals
		};
		print(ctx, "%s[", block.iname);
		var in_value = switch (ctx.in_value) {
		case Some(_): True;
		case _: False;
		};
		switch (remove_internals(argument_list_from_locals(ctx.in_instance_method, in_value, ctx.locals))) {
		case []: [];
		case l: print(ctx, "$%s", String.concat(", $", l));
		};
		spr(ctx, "]");
		ctx.inline_methods = @(ctx.inline_methods, ::(block, []));
	};

	public static function inline_function(ctx, args, hasthis, used_args, e) return {
		var index = ctx.inline_index;
		ctx.inline_index = +(ctx.inline_index, 1);
		var block = {
			() with iname = ^ (s_path(ctx, ctx.curclass.cl_path, ctx.curclass.cl_extern, ctx.curclass.cl_pos), ^ ("_", string_of_int(index)));
			iindex = index;
			ihasthis = hasthis;
			iarguments = args;
			iexpr = e;
			ilocals = used_args;
			iin_block = False;
			iinv_locals = used_args
		};
		ctx.inline_methods = @(ctx.inline_methods, ::(block, []));
		block.iname;
	};

	public static function canbe_ternary_param(e) return {
		switch (e.eexpr) {
		case TTypeExpr(_) | TConst(_) | TLocal(_) | TField(_, FEnum(_)) | TParenthesis(_) | TMeta(_) | TObjectDecl(_) | TArrayDecl(_) | TCall(_) | TUnop(_) | TNew(_) | TCast(_, _) | TBlock(::(_, []))
				: True;
		case TIf(_, e, eelse): cangen_ternary(e, eelse);
		case _: False;
		};
	};

	public static function cangen_ternary(e, eelse) return {
		switch (eelse) {
		case Some(other): && (canbe_ternary_param(e), canbe_ternary_param(other));
		case _: False;
		};
	};

	public static function gen_value(ctx, e) return {
		switch (e.eexpr) {
		case TTypeExpr(_) | TConst(_) | TLocal(_) | TArray(_) | TBinop(_) | TEnumParameter(_) | TField(_) | TParenthesis(_) | TObjectDecl(_) | TArrayDecl(_) | TCall(_) | TUnop(_) | TNew(_) | TFunction(_)
				: gen_expr(ctx, e);
		case TMeta(_, e1): gen_value(ctx, e1);
		case TBlock([]): [];
		case TCast(e, _) | TBlock(::(e, [])): gen_value(ctx, e);
		case TIf(cond, e, eelse) if (cangen_ternary(e, eelse)): spr(ctx, "[");
			gen_value(ctx, cond);
			spr(ctx, " ? ");
			gen_value(ctx, e);
			switch (eelse) {
			case Some(e): spr(ctx, " : ");
				gen_value(ctx, e);
			case _: [];
			};
			spr(ctx, "]");
		case TBlock(_) | TBreak | TContinue | TVar(_) | TReturn(_) | TWhile(_) | TThrow(_) | TSwitch(_) | TFor(_) | TIf(_) | TTry(_)
				: inline_block(ctx, e);
		};
	};

	public static function is_instance_method_defined(cls, m) return {
		if (PMap.exists(m, cls.cl_fields)) {
			True;
		} else {
			switch (cls.cl_super) {
			case Some(scls, _): is_instance_method_defined(scls, m);
			case None: False;
			};
		};
	};

	public static function is_method_defined(ctx, m, static) return {
		if (static) {
			PMap.exists(m, ctx.curclass.cl_statics);
		} else {
			is_instance_method_defined(ctx.curclass, m);
		};
	};

	public static function generate_self_method(ctx, rights, m, static, setter) return {
		if (setter) {
			if (static) {
				print(ctx, "%s function %s[$v] { return call_user_func[self::$%s, $v]; }", rights, s_ident(m), s_ident(m));
			} else {
				print(ctx, "%s function %s[$v] { return call_user_func[$this->%s, $v]; }", rights, s_ident(m), s_ident(m));
			};
		} else {
			if (static) {
				print(ctx, "%s function %s[] { return call_user_func[self::$%s]; }", rights, s_ident(m), s_ident(m));
			} else {
				print(ctx, "%s function %s[] { return call_user_func[$this->%s]; }", rights, s_ident(m), s_ident(m));
			};
		};
		newline(ctx);
	};

	public static function gen_assigned_value(ctx, eo) return {
		switch (eo) {
		case Some({ eexpr = TConst(_) } = e): print(ctx, " = ");
			gen_value(ctx, e);
		case _: [];
		};
	};

	public static function generate_field(ctx, static, f) return {
		if (!(is_extern_field(f))) {
			newline(ctx);
		} else {
			[];
		};
		ctx.locals = PMap.empty;
		ctx.inv_locals = PMap.empty;
		ctx.in_instance_method = !(static);
		var rights = if (static) {
			"static";
		} else {
			"public";
		};
		var p = ctx.curclass.cl_pos;
		switch (f.cf_expr) {
		case Some({ eexpr = TFunction(fd) }):
			if ( = (f.cf_name, "__construct")) {
				ctx.curmethod = "new";
			} else {
				ctx.curmethod = f.cf_name;
			};
			spr(ctx, ^ (rights, " "));
			if (is_dynamic_method(f)) {
				gen_dynamic_function(ctx, static, s_ident(f.cf_name), fd, f.cf_params, p);
			} else {
				gen_function(ctx, s_ident(f.cf_name), fd, f.cf_params, p);
			};
		case _: if (is_extern_field(f)) {
				[];
			} else {
				if (ctx.curclass.cl_interface) {
					switch ((new Tuple(follow(f.cf_type), f.cf_kind))) {
					case (TFun(args, r), Method(_)): print(ctx, "function %s[", s_ident(f.cf_name));
						concat(ctx, ", ", function (arg, o, t): s_funarg(ctx, arg, t, p, o), args);
						print(ctx, "]");
					case _: spr(ctx, "//");
						[];
					};
				} else {
					if (switch (f.cf_kind) {
					case Var(v): switch ((new Tuple(v.v_read, v.v_write))) {
							case (AccCall, AccCall): var m1 = ^ ("get_", f.cf_name);
								var m2 = ^ ("set_", f.cf_name);
								if (!(is_method_defined(ctx, m1, static))) {
									generate_self_method(ctx, rights, m1, static, False);
									print(ctx, "%s $%s", rights, s_ident(m1));
									if (!(is_method_defined(ctx, m2, static))) {
										newline(ctx);
									} else {
										[];
									};
								} else {
									[];
								};
								if (!(is_method_defined(ctx, m2, static))) {
									generate_self_method(ctx, rights, m2, static, True);
									print(ctx, "%s $%s", rights, s_ident(m2));
									newline(ctx);
								} else {
									[];
								};
								False;
							case (AccCall, _): var m = ^ ("get_", f.cf_name);
								if (!(is_method_defined(ctx, m, static))) {
									generate_self_method(ctx, rights, m, static, False);
								} else {
									[];
								};
								print(ctx, "%s $%s", rights, s_ident_field(f.cf_name));
								gen_assigned_value(ctx, f.cf_expr);
								True;
							case (_, AccCall): var m = ^ ("set_", f.cf_name);
								if (!(is_method_defined(ctx, m, static))) {
									generate_self_method(ctx, rights, m, static, True);
								} else {
									[];
								};
								print(ctx, "%s $%s", rights, s_ident_field(f.cf_name));
								gen_assigned_value(ctx, f.cf_expr);
								True;
							case _: False;
							};
						case _: False;
						}) {
						[];
					} else {
						var name = if (static) {
							s_ident(f.cf_name);
						} else {
							f.cf_name;
						};
						if (static) {
							switch (f.cf_kind) {
							case Var(_): switch (follow(f.cf_type)) {
								case TFun(_) | TDynamic(_): print(ctx,
																	  "static function %s[] { $args = func_get_args[]; return call_user_func_array[self::$%s, $args]; }", name, name);
									newline(ctx);
								case _: [];
								};
							case _: [];
							};
						} else {
							[];
						};
						print(ctx, "%s $%s", rights, name);
						gen_assigned_value(ctx, f.cf_expr);
					};
				};
			};
		};
	};

	public static function generate_static_field_assign(ctx, path, f) return {
		var p = ctx.curclass.cl_pos;
		if (!(ctx.curclass.cl_interface)) {
			switch (f.cf_expr) {
			case None: [];
			case Some(e): switch (e.eexpr) {
				case TConst(_): [];
				case TFunction(fd): switch (f.cf_kind) {
					case Var(_) if (switch (follow(f.cf_type)) {
							case TFun(_) | TDynamic(_): True;
								case _: False;
								}): newline(ctx);
						print(ctx, "%s::$%s = ", s_path(ctx, path, False, p), s_ident(f.cf_name));
						gen_value(ctx, e);
					case Method(MethDynamic): newline(ctx);
						print(ctx, "%s::$%s = ", s_path(ctx, path, False, p), s_ident(f.cf_name));
						gen_value(ctx, e);
					case _: [];
					};
				case _ if (is_extern_field(f)): [];
				case _: newline(ctx);
					print(ctx, "%s::$%s = ", s_path(ctx, path, False, p), s_ident(f.cf_name));
					gen_value(ctx, e);
				};
			};
		} else {
			[];
		};
	};

	public static function super_has_dynamic(c) return {
		switch (c.cl_super) {
		case None: False;
		case Some(csup, _): switch (csup.cl_dynamic) {
			case Some(_): True;
			case _: super_has_dynamic(csup);
			};
		};
	};

	public static function generate_inline_method(ctx, c, m) return {
		switch (ctx.inline_methods) {
		case []: [];
		case ::(h, t): ctx.inline_methods = t;
		};
		ctx.curclass = c;
		var old = save_locals(ctx);
		ctx.in_value = Some(m.iname);
		ctx.in_block = m.iin_block;
		ctx.in_loop = False;
		ctx.locals = m.ilocals;
		ctx.inv_locals = m.iinv_locals;
		newline(ctx);
		print(ctx, "function %s[", m.iname);
		var in_value = switch (ctx.in_value) {
		case Some(_): True;
		case _: False;
		};
		var arguments = remove_internals(argument_list_from_locals(m.ihasthis, in_value, ctx.locals));
		var arguments = switch (arguments) {
		case ::(h, []) if (=(h, "this")): ::("__hx__this", []);
		case ::(h, t) if (=(h, "this")): ::("__hx__this", t);
		case _: arguments;
		};
		var marguments = List.map(define_local(ctx), m.iarguments);
		var arguments = @(List.map(function a: ^ ("&$", a), arguments), List.map(function a: ^ ("$", a), marguments));
		switch (arguments) {
		case []: [];
		case l: spr(ctx, String.concat(", ", arguments));
		};
		spr(ctx, "] {");
		ctx.nested_loops = -(ctx.nested_loops, 1);
		var block = open_block(ctx);
		newline(ctx);
		gen_expr(ctx, m.iexpr);
		block([]);
		old([]);
		ctx.nested_loops = +(ctx.nested_loops, 1);
		newline(ctx);
		spr(ctx, "}");
	};

	public static function generate_class(ctx, c) return {
		var requires_constructor = ref(True);
		ctx.curclass = c;
		ctx.local_types = List.map(snd, c.cl_params);
		print(ctx, "%s %s ", if (c.cl_interface) {
		"interface";
	} else {
		"class";
	}, s_path(ctx, c.cl_path, c.cl_extern, c.cl_pos));
		switch (c.cl_super) {
		case None: [];
		case Some(csup, _): requires_constructor.val = False;
			print(ctx, "extends %s ", s_path(ctx, csup.cl_path, csup.cl_extern, c.cl_pos));
		};
		var implements = ExtList.List.unique(cmp = function a: function b: = (fst(a).cl_path, fst(b).cl_path), c.cl_implements);
		switch (implements) {
		case []: [];
		case l: spr(ctx, if (c.cl_interface) {
			"extends ";
		} else {
			"implements ";
		});
			concat(ctx, ", ", function (i, _): print(ctx, "%s", s_path(ctx, i.cl_path, i.cl_extern, c.cl_pos)), l);
		};
		spr(ctx, "{");
		var get_dynamic_methods = List.filter(is_dynamic_method, c.cl_ordered_fields);
		if (!(ctx.curclass.cl_interface)) {
			ctx.dynamic_methods = get_dynamic_methods;
		} else {
			[];
		};
		var cl = open_block(ctx);
		switch (c.cl_constructor) {
		case None: if ( && (requires_constructor.val, !(c.cl_interface))) {
				newline(ctx);
				spr(ctx, "public function __construct[]{}");
			} else {
				[];
			};
		case Some(f): var f = { (f) with cf_name = "__construct";
									cf_public = True
								  };
			ctx.constructor_block = True;
			generate_field(ctx, False, f);
		};
		List.iter(generate_field(ctx, False), c.cl_ordered_fields);
		switch (c.cl_dynamic) {
		case Some(_) if (&&(!(c.cl_interface), !(super_has_dynamic(c)))): newline(ctx);
			spr(ctx, "public $__dynamics = array[];\n\tpublic function __get[$n] {\n\t\tif[isset[$this->__dynamics[$n]]]\n\t\t\treturn $this->__dynamics[$n];\n\t}\n\tpublic function __set[$n, $v] {\n\t\t$this->__dynamics[$n] = $v;\n\t}\n\tpublic function __call[$n, $a] {\n\t\tif[isset[$this->__dynamics[$n]] && is_callable[$this->__dynamics[$n]]]\n\t\t\treturn call_user_func_array[$this->__dynamics[$n], $a];\n\t\tif['toString' == $n]\n\t\t\treturn $this->__toString[];\n\t\tthrow new HException[\"Unable to call <\".$n.\">\"];\n\t}");
		case Some(_) | _: if ( > (List.length(ctx.dynamic_methods), 0)) {
				newline(ctx);
				spr(ctx, "public function __call[$m, $a] {\n\t\tif[isset[$this->$m] && is_callable[$this->$m]]\n\t\t\treturn call_user_func_array[$this->$m, $a];\n\t\telse if[isset[$this->__dynamics[$m]] && is_callable[$this->__dynamics[$m]]]\n\t\t\treturn call_user_func_array[$this->__dynamics[$m], $a];\n\t\telse if['toString' == $m]\n\t\t\treturn $this->__toString[];\n\t\telse\n\t\t\tthrow new HException['Unable to call <'.$m.'>'];\n\t}");
			} else {
				[];
			};
		};
		List.iter(generate_field(ctx, True), c.cl_ordered_statics);
		function gen_props(props) return {
			String.concat(",", List.map(function (p, v): ^ ("\"", ^ (p, ^ ("\" => \"", ^ (v, "\"")))), props));
		};
		function fields(c) return {
			var list = Codegen.get_properties(@(c.cl_ordered_statics, c.cl_ordered_fields));
			switch (c.cl_super) {
			case Some(csup, _): @(list, fields(csup));
			case None: list;
			};
		};
		if (!(c.cl_interface)) {
			switch (fields(c)) {
			case []: [];
			case props: newline(ctx);
				print(ctx, "static $__properties__ = array[%s]", gen_props(props));
			};
		} else {
			[];
		};
		cl([]);
		newline(ctx);
		if (PMap.exists("__toString", c.cl_fields)) {
			[];
		} else {
			if ( && (PMap.exists("toString", c.cl_fields), && (!(c.cl_interface), !(c.cl_extern)))) {
				print(ctx, "\tfunction __toString[] { return $this->toString[]; }");
				newline(ctx);
			} else {
				if ( && (!(c.cl_interface), !(c.cl_extern))) {
					print(ctx, "\tfunction __toString[] { return '%s'; }", s_path_haxe(c.cl_path));
					newline(ctx);
				} else {
					[];
				};
			};
		};
		print(ctx, "}");
	};

	public static function createmain(com, e) return {
		var filename = switch (com.php_front) {
		case None: "index.php";
		case Some(n): n;
		};
		var ctx = { () with com = com;
					stack = stack_init(com, False);
					tabs = "";
					ch = open_out( ^ (com.file, ^ ("/", filename)));
					path = (new Tuple([], ""));
					buf = Buffer.create(lsl(1, 14));
					in_value = None;
					in_loop = False;
					in_instance_method = False;
					imports = Hashtbl.create(0);
					extern_required_paths = [];
					extern_classes_with_init = [];
					curclass = null_class;
					curmethod = "";
					locals = PMap.empty;
					inv_locals = PMap.empty;
					local_types = [];
					inits = [];
					constructor_block = False;
					dynamic_methods = [];
					all_dynamic_methods = [];
					is_call = False;
					cwd = "";
					inline_methods = [];
					nested_loops = 0;
					inline_index = 0;
					in_block = False;
		lib_path = switch (com.php_lib) {
	case None: "lib";
	case Some(s): s;
		}
				  };
		spr(ctx, "if[version_compare[PHP_VERSION, '5.1.0', '<']] {
			exit['Your current PHP version is: ' . PHP_VERSION . '. Haxe/PHP generates code for version 5.1.0 or later'];
		}");
		newline(ctx);
		newline(ctx);
		spr(ctx, ^ ("require_once dirname[__FILE__].'/", ^ (ctx.lib_path, ^ ("/php/", prefix_class(com, "Boot.class.php';\n\n")))));
		gen_value(ctx, e);
		newline(ctx);
		spr(ctx, "\n?>");
		close(ctx);
	};

	public static function generate_main(ctx, c) return {
		switch (c.cl_ordered_statics) {
		case ::( {
				cf_expr = Some(e)
			}, []): gen_value(ctx, e);
		case _: assert False;
		};
		newline(ctx);
	};

	public static function generate_enum(ctx, e) return {
		ctx.local_types = List.map(snd, e.e_params);
		var pack = open_block(ctx);
		var ename = s_path(ctx, e.e_path, e.e_extern, e.e_pos);
		print(ctx, "class %s extends Enum {", ename);
		PMap.iter(function _: function c: newline(ctx);
		switch (c.ef_type) {
	case TFun(args, _): print(ctx, "public static function %s[$", s_ident(c.ef_name));
			concat(ctx, ", $", function (a, o, t): spr(ctx, a);
			if (o) {
			spr(ctx, " = null");
			} else {
				[];
			}, args);
			spr(ctx, "] {");
			print(ctx, " return new %s[\"%s\", %d, array[$", ename, s_ident(c.ef_name), c.ef_index);
			concat(ctx, ", $", function (a, _, _): spr(ctx, a), args);
			print(ctx, "]]; }");
		case _: print(ctx, "public static $%s", s_ident(c.ef_name));
		}, e.e_constrs);
		newline(ctx);
		spr(ctx, "public static $__constructors = array[");
		var first = ref(True);
		PMap.iter(function _: function c:
		if (!(first.val)) {
		spr(ctx, ", ");
		} else {
			[];
		};
		print(ctx, "%d => '%s'", c.ef_index, s_ident(c.ef_name));
		first.val = False, e.e_constrs);
		spr(ctx, "]");
		newline(ctx);
		switch (Codegen.build_metadata(ctx.com, TEnumDecl(e))) {
		case None: [];
		case Some(_): spr(ctx, "public static $__meta__");
			newline(ctx);
		};
		pack([]);
		print(ctx, "}");
		PMap.iter(function _: function c:
		switch (c.ef_type) {
	case TFun(args, _): [];
		case _: newline(ctx);
			print(ctx, "%s::$%s = new %s[\"%s\", %d]", ename, s_ident(c.ef_name), ename, c.ef_name, c.ef_index);
		}, e.e_constrs);
		newline(ctx);
		switch (Codegen.build_metadata(ctx.com, TEnumDecl(e))) {
		case None: [];
		case Some(e): print(ctx, "%s::$__meta__ = ", ename);
			gen_expr(ctx, e);
			newline(ctx);
		};
	};

	public static function generate(com) return {
		var all_dynamic_methods = ref([]);
		var extern_classes_with_init = ref([]);
		var php_lib_path = switch (com.php_lib) {
		case None: "lib";
		case Some(n): n;
		};
		create_directory(com, Str.split(Str.regexp("/"), php_lib_path));
		function check_class_fields(c) return {
			var lc_names = ref([]);
			var special_cases = ::("toString", []);
			function loop(c, lst, static) return {
				function in_special_cases(name) return {
					List.exists(function n: = (String.lowercase(n), name), @(special_cases, List.map(function f: f.cf_name, c.cl_overrides)));
				};
				List.iter(function cf: var name = String.lowercase(cf.cf_name);
				function prefixed_name(s) return {
					^ (if (s) {
					"s_";
				} else {
					"i_";
				}, name);
				};
				switch ((new Tuple(cf.cf_kind, cf.cf_expr))) {
			case (Method(_), Some(e)) if (!(in_special_cases(name))):
					try {
						var lc = List.find(function n: var n = snd(n);
						if (static) {
							= (n, prefixed_name(False));
						} else {
							|| ( = (n, prefixed_name(False)), = (n, prefixed_name(True)));
						}, lc_names.val);
						unsupported( ^ ("method '", ^ (s_type_path(c.cl_path), ^ (".", ^ (cf.cf_name, ^ ("' already exists here '", ^ (fst(lc),
													   "' [different case?]")))))), c.cl_pos);
					} catch (e: Not_found) {
						lc_names.val = ::((new Tuple( ^ (s_type_path(c.cl_path), ^ (".", cf.cf_name)), prefixed_name(static))), lc_names.val);
					};
				case _: [];
				}, lst);
			};
			function _check_class_fields(cl) return {
				switch (cl.cl_super) {
				case Some(s, _): _check_class_fields(s);
				case _: [];
				};
				loop(cl, cl.cl_ordered_statics, True);
				loop(cl, cl.cl_ordered_fields, False);
			};
			_check_class_fields(c);
		};
		List.iter(function t:
		switch (t) {
	case TClassDecl(c): check_class_fields(c);
		case TEnumDecl(e): var e_names = ref([]);
			List.iter(function en:
			if (List.exists(function n: = (n, String.lowercase(en)), e_names.val)) {
			unsupported( ^ ("'", ^ (en, "' constructor exists with different case")), e.e_pos);
			} else {
				e_names.val = ::(String.lowercase(en), e_names.val);
			}, e.e_names);
		case _: [];
		}, com.types);
		List.iter(function t:
		switch (t) {
	case TClassDecl(c): 	function dynamic_methods_names(lst) return {
				List.map(function fd: {
					() with mpath = c.cl_path;
					mname = fd.cf_name
				}, List.filter(is_dynamic_method, lst));
			};
			all_dynamic_methods.val = @(dynamic_methods_names(c.cl_ordered_fields), all_dynamic_methods.val);
			if (c.cl_extern) {
				switch (c.cl_init) {
				case Some(_): extern_classes_with_init.val = ::(c.cl_path, extern_classes_with_init.val);
				case _: [];
				};
			} else {
				all_dynamic_methods.val = @(dynamic_methods_names(c.cl_ordered_statics), all_dynamic_methods.val);
			};
		case _: [];
		}, com.types);
		List.iter(Codegen.fix_abstract_inheritance(com), com.types);
		List.iter(function t:
		switch (t) {
	case TClassDecl(c): if (c.cl_extern) {
				switch (c.cl_init) {
				case None: [];
				case Some(e): var ctx = init(com, php_lib_path, c.cl_path, 3);
					gen_expr(ctx, e);
					newline(ctx);
					close(ctx);
				};
			} else {
				var ctx = init(com, php_lib_path, c.cl_path, if (c.cl_interface) {
				2;
			} else {
				0;
			});
				ctx.extern_classes_with_init = extern_classes_with_init.val;
				ctx.all_dynamic_methods = all_dynamic_methods.val;
				generate_class(ctx, c);
				switch (c.cl_init) {
				case None: [];
				case Some(e): newline(ctx);
					gen_expr(ctx, e);
				};
				List.iter(generate_static_field_assign(ctx, c.cl_path), c.cl_ordered_statics);
				if ( && ( = (c.cl_path, (new Tuple(::("php", []), "Boot"))), com.debug)) {
					newline(ctx);
					print(ctx, "$%s = new _hx_array[array[]]", ctx.stack.Codegen.stack_var);
					newline(ctx);
					print(ctx, "$%s = new _hx_array[array[]]", ctx.stack.Codegen.stack_exc_var);
				} else {
					[];
				};
				function loop(l) return {
					switch (l) {
					case []: [];
					case ::(h, _): generate_inline_method(ctx, c, h);
						loop(ctx.inline_methods);
					};
				};
				loop(ctx.inline_methods);
				newline(ctx);
				close(ctx);
			};
		case TEnumDecl(e): if (e.e_extern) {
				[];
			} else {
				var ctx = init(com, php_lib_path, e.e_path, 1);
				generate_enum(ctx, e);
				close(ctx);
			};
		case TTypeDecl(_) | TAbstractDecl(_): [];
		}, com.types);
		switch (com.main) {
		case None: [];
		case Some(e): createmain(com, e);
		};
		Hashtbl.iter(function name: function data: write_resource(com.file, name, data), com.resources);
	}
}
;
