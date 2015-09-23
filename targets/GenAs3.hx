import Type;
import Common;

typedef Context_infos = {
	com : Common.Context
};

typedef Context = {
	inf : Context_infos,
	ch : Out_channel,
	buf : Buffer,
	path : Path,
	get_sets : Hashtbl<Tuple<String, Bool>, String>,
	curclass : Tclass,
	tabs : String,
	in_value : Option<Tvar>,
	in_static : Bool,
	handle_break : Bool,
	imports : Hashtbl<String, List<String>>,
	gen_uid : Int,
	local_types : List<T>,
	constructor_block : Bool,
	block_inits : Option < Unit -> Unit >
};

class Genas3 {
	public static var follow = Abstract.follow_with_abstracts;

	public static function is_var_field(f) return {
		switch (f) {
		case FStatic(_, f) | FInstance(_, _, f): switch (f.cf_kind) {
			case Var(_) | Method(MethDynamic): True;
			case _: False;
			};
		case _: False;
		};
	};

	public static function is_special_compare(e1, e2) return {
		switch ((new Tuple(e1.eexpr, e2.eexpr))) {
		case (TConst(TNull), _) | (_, TConst(TNull)): None;
		case _: switch ((new Tuple(follow(e1.etype), follow(e2.etype)))) {
			case (TInst({ cl_path = (::(flash, []), NativeXml) } = c, _), _) | (_, TInst({ cl_path = (::(flash, []), NativeXml) } = c, _))
				: Some(c);
			case _: None;
			};
		};
	};

	public static function is_fixed_override(cf, t) return {
		function is_type_parameter(c) return {
			switch (c.cl_kind) {
			case KTypeParameter(_): True;
			case _: False;
			};
		};
		switch ((new Tuple(follow(cf.cf_type), follow(t)))) {
		case (TFun(_, r1), TFun(_, r2)): switch ((new Tuple(follow(r1), follow(r2)))) {
			case (TInst(c1, _), TInst(c2, _)) if (&&(!=(c1, c2), &&(!(is_type_parameter(c1)), !(is_type_parameter(c2))))): True;
			case _: False;
			};
		case _: False;
		};
	};

	public static function protect(name) return {
		switch (name) {
		case Error | Namespace: ^ ("_", name);
		case _: name;
		};
	};

	public static function s_path(ctx, stat, path, p) return {
		switch (path) {
		case ([], name): switch (name) {
			case Int: "int";
			case Float: "Number";
			case Dynamic: "Object";
			case Bool: "Boolean";
			case Enum: "Class";
			case EnumValue: "enum";
			case _: name;
			};
		case (::(flash, []), FlashXml__): "Xml";
		case (::(flash, ::(errors, [])), Error): "Error";
		case (::(flash, []), Vector): "Vector";
		case (::(flash, ::(xml, [])), XML): "XML";
		case (::(flash, ::(xml, [])), XMLList): "XMLList";
		case (::(flash, ::(utils, [])), QName): "QName";
		case (::(flash, ::(utils, [])), Namespace): "Namespace";
		case (::(haxe, []), Int32) if (!(stat)): "int";
		case (pack, name): var name = protect(name);
			var packs = try {
				Hashtbl.find(ctx.imports, name);
			} catch (e: Not_found) {
				[];
			};
			if (!(List.mem(pack, packs))) {
				Hashtbl.replace(ctx.imports, name, ::(pack, packs));
			} else {
				[];
			};
			Ast.s_type_path((new Tuple(pack, name)));
		};
	};

	public static var reserved = var h = Hashtbl.create(0);
	List.iter(function l: Hashtbl.add(h, l, []), ::("is", ::("as", ::("int", ::("uint", ::("const", ::("getTimer", ::("typeof",
			  ::("parseInt", ::("parseFloat", ::("finally", ::("with", ::("final", ::("internal", ::("native", ::("namespace",
								::("include", ::("delete", ::("print", ::("trace", ::("function", ::("class", ::("var", ::("if", ::("else", ::("while",
										::("do", ::("for", ::("break", ::("continue", ::("return", ::("extends", ::("implements", ::("import", ::("switch",
												::("case", ::("default", ::("static", ::("public", ::("private", ::("try", ::("catch", ::("new", ::("this", ::("throw",
														::("interface", ::("override", ::("package", ::("null", ::("true", ::("false",
																::("void", []))))))))))))))))))))))))))))))))))))))))))))))))))));
	h;

	public static function s_ident(n) return {
		if (Hashtbl.mem(reserved, n)) {
			^ ("_", n);
		} else {
			n;
		};
	};

	public static function valid_as3_ident(s) return {
		try {
			for (i in /*to*/0... - (String.length(s), 1)) {
				switch (String.unsafe_get(s, i)) {
				case 'a' .. 'z' | 'A' .. 'Z' | '$' | '_': [];
				case '0' .. '9' if (>(i, 0)): [];
				case _: raise(Exit);
				};
			};
			True;
		} catch (e: Exit) {
			False;
		};
	};

	public static function anon_field(s) return {
		var s = s_ident(s);
		if (!(valid_as3_ident(s))) {
			^ ("\"", ^ (s, "\""));
		} else {
			s;
		};
	};

	public static function create_dir(acc) return {
	case []: [];
	case ::(d, l): var dir = String.concat("/", List.rev(::(d, acc)));
		if (!(Sys.file_exists(dir))) {
			Unix.mkdir(dir, 0o755);
		} else {
			[];
		};
		create_dir(::(d, acc), l);
	};

	public static function init(infos, path) return {
		var dir = ::(infos.com.file, fst(path));
		create_dir([], dir);
		var ch = open_out( ^ (String.concat("/", dir), ^ ("/", ^ (snd(path), ".as"))));
		var imports = Hashtbl.create(0);
		Hashtbl.add(imports, snd(path), ::(fst(path), []));
		{
			() with inf = infos;
			tabs = "";
			ch = ch;
			path = path;
			buf = Buffer.create(lsl(1, 14));
			in_value = None;
			in_static = False;
			handle_break = False;
			imports = imports;
			curclass = null_class;
			gen_uid = 0;
			local_types = [];
			get_sets = Hashtbl.create(0);
			constructor_block = False;
			block_inits = None
		};
	};

	public static function close(ctx) return {
		switch (ctx.inf.com.main_class) {
		case Some(tp) if (=(tp, ctx.curclass.cl_path)): output_string(ctx.ch, "// Compile __main__.as instead\n");
		case _: [];
		};
		output_string(ctx.ch, Printf.sprintf("package %s {\n", String.concat(".", fst(ctx.path))));
		Hashtbl.iter(function name: function paths: List.iter(function pack: var path = (new Tuple(pack, name));
		if (<>(path, ctx.path)) {
		output_string(ctx.ch, ^ ("\timport ", ^ (Ast.s_type_path(path), ";\n")));
		} else {
			[];
		}, paths), ctx.imports);
		output_string(ctx.ch, Buffer.contents(ctx.buf));
		close_out(ctx.ch);
	};

	public static function gen_local(ctx, l) return {
		ctx.gen_uid = +(ctx.gen_uid, 1);
		if ( = (ctx.gen_uid, 1)) {
			l;
		} else {
			^ (l, string_of_int(ctx.gen_uid));
		};
	};

	public static function spr(ctx, s) return {
		Buffer.add_string(ctx.buf, s);
	};

	public static function print(ctx) return {
		Printf.kprintf(function s: Buffer.add_string(ctx.buf, s));
	};

	public static function unsupported(p) return {
		error("This expression cannot be generated to AS3", p);
	};

	public static function newline(ctx) return {
		function loop(p) return {
			switch (Buffer.nth(ctx.buf, p)) {
			case '}' | '{' | ':' | ';': print(ctx, "\n%s", ctx.tabs);
			case '\n' | '\t': loop(-(p, 1));
			case _: print(ctx, ";\n%s", ctx.tabs);
			};
		};
		loop(-(Buffer.length(ctx.buf), 1));
	};

	public static function block_newline(ctx) return {
		switch (Buffer.nth(ctx.buf, -(Buffer.length(ctx.buf), 1))) {
		case '}': print(ctx, ";\n%s", ctx.tabs);
		case _: newline(ctx);
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

	public static function default_value(tstr) return {
		switch (tstr) {
		case int | uint: "0";
		case Number: "NaN";
		case Boolean: "false";
		case _: "null";
		};
	};

	public static function type_str(ctx, t, p) return {
		switch (t) {
		case TEnum(_) | TInst(_) if (List.memq(t, ctx.local_types)): "*";
		case TAbstract(a, pl) if (!(Ast.Meta.has(Ast.Meta.CoreType, a.a_meta))): type_str(ctx, Abstract.get_underlying_type(a, pl),
			p);
		case TAbstract(a, _): switch (a.a_path) {
			case ([], Void): "void";
			case ([], UInt): "uint";
			case ([], Int): "int";
			case ([], Float): "Number";
			case ([], Bool): "Boolean";
			case _: s_path(ctx, True, a.a_path, p);
			};
		case TEnum(e, _): if (e.e_extern) {
				switch (e.e_path) {
				case ([], Void): "void";
				case ([], Bool): "Boolean";
				case _: function loop(match) return switch (match) {
					case []: "Object";
					case ::((Ast.Meta.FakeEnum, ::((Ast.EConst(Ast.Ident(n)), _), []), _), _): switch (n) {
						case Int: "int";
						case UInt: "uint";
						case _: n;
						};
					case ::(_, l): loop(l);
					};
					loop(e.e_meta);
				};
			} else {
				s_path(ctx, True, e.e_path, p);
			};
		case TInst({ cl_path = (::(flash, []), Vector) }, ::(pt, [])):
			switch (pt) {
			case TInst({ cl_kind = KTypeParameter(_) }, _): "*";
			case _: ^ ("Vector.<", ^ (type_str(ctx, pt, p), ">"));
			};
		case TInst(c, _): switch (c.cl_kind) {
			case KNormal | KGeneric | KGenericInstance(_) | KAbstractImpl(_): s_path(ctx, False, c.cl_path, p);
			case KTypeParameter(_) | KExtension(_) | KExpr(_) | KMacroType | KGenericBuild(_): "*";
			};
		case TFun(_): "Function";
		case TMono(r): switch (r.val) {
			case None: "*";
			case Some(t): type_str(ctx, t, p);
			};
		case TAnon(_) | TDynamic(_): "*";
		case TType(t, args): switch (t.t_path) {
			case ([], UInt): "uint";
			case ([], Null): switch (args) {
				case ::(t, []): switch (follow(t)) {
					case TAbstract({ a_path = ([], UInt) }, _) | TAbstract({ a_path = ([], Int) }, _) | TAbstract({ a_path = ([], Float) }, _) | TAbstract({ a_path = ([], Bool) }, _) | TInst({ cl_path = ([], Int) }, _) | TInst({ cl_path = ([], Float) }, _) | TEnum({ e_path = ([], Bool) }, _)
							: "*";
					case _: type_str(ctx, t, p);
					};
				case _: assert False;
				};
			case _: type_str(ctx, apply_params(t.t_params, args, t.t_type), p);
			};
		case TLazy(f): type_str(ctx, f.val([]), p);
		};
	};

	public static function iter_switch_break(in_switch, e) return {
		switch (e.eexpr) {
		case TFunction(_) | TWhile(_) | TFor(_): [];
		case TSwitch(_) if (!(in_switch)): iter_switch_break(True, e);
		case TBreak if (in_switch): raise(Exit);
		case _: iter(iter_switch_break(in_switch), e);
		};
	};

	public static function handle_break(ctx, e) return {
		var old_handle = ctx.handle_break;
		try {
			iter_switch_break(False, e);
			ctx.handle_break = False;
			function []: ctx.handle_break = old_handle;
		} catch (e: Exit) {
			spr(ctx, "try {");
			var b = open_block(ctx);
			newline(ctx);
			ctx.handle_break = True;
			function []: b([]);
			ctx.handle_break = old_handle;
			newline(ctx);
			spr(ctx, "} catch[ e : * ] { if[ e != \"__break__\" ] throw e; }");
		};
	};

	public static function this(ctx) return {
		if (<>(ctx.in_value, None)) {
			"$this";
		} else {
			"this";
		};
	};

	public static function generate_resources(infos) return {
		if (<>(Hashtbl.length(infos.com.resources), 0)) {
			var dir = ::(infos.com.file, ::("__res", []));
			create_dir([], dir);
			function add_resource(name, data) return {
				var name = Base64.str_encode(name);
				var ch = open_out_bin(String.concat("/", @(dir, ::(name, []))));
				output_string(ch, data);
				close_out(ch);
			};
			Hashtbl.iter(function name: function data: add_resource(name, data), infos.com.resources);
			var ctx = init(infos, (new Tuple([], "__resources__")));
			spr(ctx, "\timport flash.utils.Dictionary;\n");
			spr(ctx, "\tpublic class __resources__ {\n");
			spr(ctx, "\t\tpublic static var list:Dictionary;\n");
			var inits = ref([]);
			var k = ref(0);
			Hashtbl.iter(function name: function _: var varname = ^ ("v", string_of_int(k.val));
						 k.val = +(k.val, 1);
						 print(ctx, "\t\t[Embed[source = \"__res/%s\", mimeType = \"application/octet-stream\"]]\n", Base64.str_encode(name));
						 print(ctx, "\t\tpublic static var %s:Class;\n", varname);
						 inits.val = ::( ^ ("list[\"", ^ (Ast.s_escape(name), ^ ("\"] = ", ^ (varname, ";")))), inits.val), infos.com.resources);
			spr(ctx, "\t\tstatic public function __init__[]:void {\n");
			spr(ctx, "\t\t\tlist = new Dictionary[];\n");
			List.iter(function init: print(ctx, "\t\t\t%s\n", init), inits.val);
			spr(ctx, "\t\t}\n");
			spr(ctx, "\t}\n");
			spr(ctx, "}");
			close(ctx);
		} else {
			[];
		};
	};

	public static function gen_constant(ctx, p) return {
	case TInt(i): print(ctx, "%ld", i);
	case TFloat(s): spr(ctx, s);
	case TString(s): print(ctx, "\"%s\"", Ast.s_escape(s));
	case TBool(b): spr(ctx, if (b) {
		"true";
	} else {
		"false";
	});
	case TNull: spr(ctx, "null");
	case TThis: spr(ctx, this(ctx));
	case TSuper: spr(ctx, "super");
	};

	public static function gen_function_header(ctx, name, f, params, p) return {
		var old = ctx.in_value;
		var old_t = ctx.local_types;
		var old_bi = ctx.block_inits;
		ctx.in_value = None;
		ctx.local_types = @(List.map(snd, params), ctx.local_types);
		function init([]) return {
			List.iter(function (v, o):
			switch (o) {
		case Some(c) if (&&(is_nullable(v.v_type), <>(c, TNull))): newline(ctx);
				print(ctx, "if[%s==null] %s=", v.v_name, v.v_name);
				gen_constant(ctx, p, c);
			case _: [];
			}, f.tf_args);
			ctx.block_inits = None;
		};
		ctx.block_inits = Some(init);
		print(ctx, "function%s[", switch (name) {
	case None: "";
	case Some(n, meta): function loop(match) return switch (match) {
			case []: n;
			case ::((Ast.Meta.Getter, ::((Ast.EConst(Ast.Ident(i)), _), []), _), _): ^ ("get ", i);
			case ::((Ast.Meta.Setter, ::((Ast.EConst(Ast.Ident(i)), _), []), _), _): ^ ("set ", i);
			case ::(_, l): loop(l);
			};
			^ (" ", loop(meta));
		});
		concat(ctx, ",", function (v, c):
		switch (v.v_name) {
	case __arguments__: print(ctx, "...__arguments__");
		case _: var tstr = type_str(ctx, v.v_type, p);
			print(ctx, "%s : %s", s_ident(v.v_name), tstr);
			switch (c) {
			case None: if (ctx.constructor_block) {
					print(ctx, " = %s", default_value(tstr));
				} else {
					[];
				};
			case Some(c): spr(ctx, " = ");
				gen_constant(ctx, p, c);
			};
		}, f.tf_args);
		print(ctx, "] : %s ", type_str(ctx, f.tf_type, p));
		function []: ctx.in_value = old;
		ctx.local_types = old_t;
		ctx.block_inits = old_bi;
	};

	public static function gen_call(ctx, e, el, r) return {
		switch ((new Tuple(e.eexpr, el))) {
		case (TCall(x, _), el): spr(ctx, "[");
			gen_value(ctx, e);
			spr(ctx, "]");
			spr(ctx, "[");
			concat(ctx, ",", gen_value(ctx), el);
			spr(ctx, "]");
		case (TLocal({ v_name = __is__ }), ::(e1, ::(e2, []))): gen_value(ctx, e1);
			spr(ctx, " is ");
			gen_value(ctx, e2);
		case (TLocal({ v_name = __in__ }), ::(e1, ::(e2, []))): spr(ctx, "[");
			gen_value(ctx, e1);
			spr(ctx, " in ");
			gen_value(ctx, e2);
			spr(ctx, "]");
		case (TLocal({ v_name = __as__ }), ::(e1, ::(e2, []))): gen_value(ctx, e1);
			spr(ctx, " as ");
			gen_value(ctx, e2);
		case (TLocal({ v_name = __int__ }), ::(e, [])): spr(ctx, "int[");
			gen_value(ctx, e);
			spr(ctx, "]");
		case (TLocal({ v_name = __float__ }), ::(e, [])): spr(ctx, "Number[");
			gen_value(ctx, e);
			spr(ctx, "]");
		case (TLocal({ v_name = __typeof__ }), ::(e, [])): spr(ctx, "typeof ");
			gen_value(ctx, e);
		case (TLocal({ v_name = __keys__ }), ::(e, [])): var ret = switch (ctx.in_value) {
			case None: assert False;
			case Some(r): r;
			};
			print(ctx, "%s = new Array[]", ret.v_name);
			newline(ctx);
			var tmp = gen_local(ctx, "$k");
			print(ctx, "for[var %s : String in ", tmp);
			gen_value(ctx, e);
			print(ctx, "] %s.push[%s]", ret.v_name, tmp);
		case (TLocal({ v_name = __hkeys__ }), ::(e, [])): var ret = switch (ctx.in_value) {
			case None: assert False;
			case Some(r): r;
			};
			print(ctx, "%s = new Array[]", ret.v_name);
			newline(ctx);
			var tmp = gen_local(ctx, "$k");
			print(ctx, "for[var %s : String in ", tmp);
			gen_value(ctx, e);
			print(ctx, "] %s.push[%s.substr[1]]", ret.v_name, tmp);
		case (TLocal({ v_name = __foreach__ }), ::(e, [])): var ret = switch (ctx.in_value) {
			case None: assert False;
			case Some(r): r;
			};
			print(ctx, "%s = new Array[]", ret.v_name);
			newline(ctx);
			var tmp = gen_local(ctx, "$k");
			print(ctx, "for each[var %s : * in ", tmp);
			gen_value(ctx, e);
			print(ctx, "] %s.push[%s]", ret.v_name, tmp);
		case (TLocal({ v_name = __new__ }), ::(e, args)): spr(ctx, "new ");
			gen_value(ctx, e);
			spr(ctx, "[");
			concat(ctx, ",", gen_value(ctx), args);
			spr(ctx, "]");
		case (TLocal({ v_name = __delete__ }), ::(e, ::(f, []))): spr(ctx, "delete[");
			gen_value(ctx, e);
			spr(ctx, "[");
			gen_value(ctx, f);
			spr(ctx, "]");
			spr(ctx, "]");
		case (TLocal({ v_name = __unprotect__ }), ::(e, [])): gen_value(ctx, e);
		case (TLocal({ v_name = __vector__ }), ::(e, [])): spr(ctx, type_str(ctx, r, e.epos));
			spr(ctx, "[");
			gen_value(ctx, e);
			spr(ctx, "]");
		case (TField(_, FStatic({ cl_path = (::(flash, []), Lib) }, { cf_name = as })), ::(e1, ::(e2, []))): gen_value(ctx, e1);
			spr(ctx, " as ");
			gen_value(ctx, e2);
		case (TField(_, FStatic({ cl_path = (::(flash, []), Vector) }, cf)), args):
			switch ((new Tuple(cf.cf_name, args))) {
			case (ofArray, ::(e, [])) | (convert, ::(e, [])): switch (follow(r)) {
				case TInst({ cl_path = (::(flash, []), Vector) }, ::(t, [])): print(ctx, "Vector.<%s>[", type_str(ctx, t, e.epos));
					gen_value(ctx, e);
					print(ctx, "]");
				case _: assert False;
				};
			case _: assert False;
			};
		case (TField(e1, FAnon({ cf_name = s }) | FDynamic(s)), ::(ef, [])) if (||(=(s, "map"), =(s, "filter"))): spr(ctx,
					s_path(ctx, True, (new Tuple(::("flash", []), "Boot")), e.epos));
			gen_field_access(ctx, t_dynamic, ^ (s, "Dynamic"));
			spr(ctx, "[");
			concat(ctx, ",", gen_value(ctx), ::(e1, ::(ef, [])));
			spr(ctx, "]");
		case (TField(ee, f), args) if (is_var_field(f)): spr(ctx, "[");
			gen_value(ctx, e);
			spr(ctx, "]");
			spr(ctx, "[");
			concat(ctx, ",", gen_value(ctx), el);
			spr(ctx, "]");
		case (TField(e1, FInstance(_, _, cf)), el) if (is_fixed_override(cf, e.etype)): var s = type_str(ctx, r, e.epos);
			spr(ctx, "[[");
			gen_value(ctx, e);
			spr(ctx, "[");
			concat(ctx, ",", gen_value(ctx), el);
			spr(ctx, "]");
			print(ctx, "] as %s]", s);
		case _: gen_value(ctx, e);
			spr(ctx, "[");
			concat(ctx, ",", gen_value(ctx), el);
			spr(ctx, "]");
		};
	};

	public static function gen_value_op(ctx, e) return {
		switch (e.eexpr) {
		case TBinop(op, _, _) if (||(=(op, Ast.OpAnd), ||(=(op, Ast.OpOr), =(op, Ast.OpXor)))): spr(ctx, "[");
			gen_value(ctx, e);
			spr(ctx, "]");
		case _: gen_value(ctx, e);
		};
	};

	public static function gen_field_access(ctx, t, s) return {
		function field(c) return {
			switch ((new Tuple(fst(c.cl_path), snd(c.cl_path), s))) {
			case ([], Math, NaN) | ([], Math, NEGATIVE_INFINITY) | ([], Math, POSITIVE_INFINITY) | ([], Math, isFinite) | ([], Math, isNaN) | ([], Date, now) | ([], Date, fromTime) | ([], Date, fromString)
					: print(ctx, "[\"%s\"]", s);
			case ([], String, charCodeAt): spr(ctx, "[\"charCodeAtHX\"]");
			case ([], Array, map): spr(ctx, "[\"mapHX\"]");
			case ([], Array, filter): spr(ctx, "[\"filterHX\"]");
			case ([], Date, toString): print(ctx, "[\"toStringHX\"]");
			case ([], String, cca): print(ctx, ".charCodeAt");
			case (::(flash, ::(xml, [])), XML, namespace): print(ctx, ".namespace");
			case _: print(ctx, ".%s", s_ident(s));
			};
		};
		switch (follow(t)) {
		case TInst(c, _): field(c);
		case TAnon(a): switch (a.a_status.val) {
			case Statics(c): field(c);
			case _: print(ctx, ".%s", s_ident(s));
			};
		case _: print(ctx, ".%s", s_ident(s));
		};
	};

	public static function gen_expr(ctx, e) return {
		switch (e.eexpr) {
		case TConst(c): gen_constant(ctx, e.epos, c);
		case TLocal(v): spr(ctx, s_ident(v.v_name));
		case TArray({ eexpr = TLocal({ v_name = __global__ }) }, { eexpr = TConst(TString(s)) }): var path = Ast.parse_path(s);
			spr(ctx, s_path(ctx, False, path, e.epos));
		case TArray(e1, e2): gen_value(ctx, e1);
			spr(ctx, "[");
			gen_value(ctx, e2);
			spr(ctx, "]");
		case TBinop(Ast.OpEq, e1, e2) if (switch (is_special_compare(e1, e2)) {
				case Some(c): True;
					case None: False;
					}): var c = switch (is_special_compare(e1, e2)) {
			case Some(c): c;
			case None: assert False;
			};
			gen_expr(ctx, mk(TCall(mk(TField(mk(TTypeExpr(TClassDecl(c)), t_dynamic, e.epos), FDynamic("compare")), t_dynamic, e.epos),
								   ::(e1, ::(e2, []))), ctx.inf.com.basic.tbool, e.epos));
		case TBinop(op, { eexpr = TField(ei, FInstance({ cl_interface = True }, _, { cf_kind = Method(MethDynamic) | Var(_); cf_name = s })) }, e2)
				: gen_value(ctx, ei);
			print(ctx, "[\"%s\"]", s);
			print(ctx, " %s ", Ast.s_binop(op));
			gen_value_op(ctx, e2);
		case TBinop(op, e1, e2): gen_value_op(ctx, e1);
			print(ctx, " %s ", Ast.s_binop(op));
			gen_value_op(ctx, e2);
		case TField(ei, FInstance({ cl_interface = True }, _, { cf_kind = Method(MethDynamic) | Var(_); cf_name = s })): spr(ctx,
					"[");
			gen_value(ctx, ei);
			print(ctx, "[\"%s\"]", s);
			print(ctx, " as %s]", type_str(ctx, e.etype, e.epos));
		case TField({ eexpr = TArrayDecl(_) } = e1, s): spr(ctx, "[");
			gen_expr(ctx, e1);
			spr(ctx, "]");
			gen_field_access(ctx, e1.etype, field_name(s));
		case TEnumParameter(e, _, i): gen_value(ctx, e);
			print(ctx, ".params[%i]", i);
		case TField(e, s): gen_value(ctx, e);
			gen_field_access(ctx, e.etype, field_name(s));
		case TTypeExpr(t): spr(ctx, s_path(ctx, True, t_path(t), e.epos));
		case TParenthesis(e): spr(ctx, "[");
			gen_value(ctx, e);
			spr(ctx, "]");
		case TMeta(_, e): gen_expr(ctx, e);
		case TReturn(eo): if (<>(ctx.in_value, None)) {
				unsupported(e.epos);
			} else {
				[];
			};
			switch (eo) {
			case None: spr(ctx, "return");
			case Some(e) if (switch (follow(e.etype)) {
					case TEnum({ e_path = ([], Void) }, []) | TAbstract({ a_path = ([], Void) }, []): True;
						case _: False;
						}): print(ctx, "{");
				var bend = open_block(ctx);
				newline(ctx);
				gen_value(ctx, e);
				newline(ctx);
				spr(ctx, "return");
				bend([]);
				newline(ctx);
				print(ctx, "}");
			case Some(e): spr(ctx, "return ");
				gen_value(ctx, e);
			};
		case TBreak: if (<>(ctx.in_value, None)) {
				unsupported(e.epos);
			} else {
				[];
			};
			if (ctx.handle_break) {
				spr(ctx, "throw \"__break__\"");
			} else {
				spr(ctx, "break");
			};
		case TContinue: if (<>(ctx.in_value, None)) {
				unsupported(e.epos);
			} else {
				[];
			};
			spr(ctx, "continue");
		case TBlock(el): print(ctx, "{");
			var bend = open_block(ctx);
			var cb = if (!(ctx.constructor_block)) {
				function []: [];
			} else {
				if (!(Codegen.constructor_side_effects(e))) {
					ctx.constructor_block = False;
					function []: [];
				} else {
					ctx.constructor_block = False;
					print(ctx, " if[ !%s.skip_constructor ] {", s_path(ctx, True, (new Tuple(::("flash", []), "Boot")), e.epos));
					function []: print(ctx, "}");
				};
			};
			switch (ctx.block_inits) {
			case None: [];
			case Some(i): i([]);
			};
			List.iter(function e: gen_block_element(ctx, e), el);
			bend([]);
			newline(ctx);
			cb([]);
			print(ctx, "}");
		case TFunction(f): var h = gen_function_header(ctx, None, f, [], e.epos);
			var old = ctx.in_static;
			ctx.in_static = True;
			gen_expr(ctx, f.tf_expr);
			ctx.in_static = old;
			h([]);
		case TCall(v, el): gen_call(ctx, v, el, e.etype);
		case TArrayDecl(el): spr(ctx, "[");
			concat(ctx, ",", gen_value(ctx), el);
			spr(ctx, "]");
		case TThrow(e): spr(ctx, "throw ");
			gen_value(ctx, e);
		case TVar(v, eo): spr(ctx, "var ");
			print(ctx, "%s : %s", s_ident(v.v_name), type_str(ctx, v.v_type, e.epos));
			switch (eo) {
			case None: [];
			case Some(e): spr(ctx, " = ");
				gen_value(ctx, e);
			};
		case TNew(c, params, el): switch ((new Tuple(c.cl_path, params))) {
			case ((::(flash, []), Vector), ::(pt, [])): print(ctx, "new Vector.<%s>[", type_str(ctx, pt, e.epos));
			case _: print(ctx, "new %s[", s_path(ctx, True, c.cl_path, e.epos));
			};
			concat(ctx, ",", gen_value(ctx), el);
			spr(ctx, "]");
		case TIf(cond, e, eelse): spr(ctx, "if");
			gen_value(ctx, parent(cond));
			spr(ctx, " ");
			gen_expr(ctx, e);
			switch (eelse) {
			case None: [];
			case Some(e): newline(ctx);
				spr(ctx, "else ");
				gen_expr(ctx, e);
			};
		case TUnop(op, Ast.Prefix, e): spr(ctx, Ast.s_unop(op));
			gen_value(ctx, e);
		case TUnop(op, Ast.Postfix, e): gen_value(ctx, e);
			spr(ctx, Ast.s_unop(op));
		case TWhile(cond, e, Ast.NormalWhile): var handle_break = handle_break(ctx, e);
			spr(ctx, "while");
			gen_value(ctx, parent(cond));
			spr(ctx, " ");
			gen_expr(ctx, e);
			handle_break([]);
		case TWhile(cond, e, Ast.DoWhile): var handle_break = handle_break(ctx, e);
			spr(ctx, "do ");
			gen_expr(ctx, e);
			spr(ctx, " while");
			gen_value(ctx, parent(cond));
			handle_break([]);
		case TObjectDecl(fields): spr(ctx, "{ ");
			concat(ctx, ", ", function (f, e): print(ctx, "%s : ", anon_field(f));
				   gen_value(ctx, e), fields);
			spr(ctx, "}");
		case TFor(v, it, e): var handle_break = handle_break(ctx, e);
			var tmp = gen_local(ctx, "$it");
			print(ctx, "{ var %s : * = ", tmp);
			gen_value(ctx, it);
			newline(ctx);
			print(ctx, "while[ %s.hasNext[] ] { var %s : %s = %s.next[]", tmp, s_ident(v.v_name), type_str(ctx, v.v_type, e.epos),
				  tmp);
			newline(ctx);
			gen_expr(ctx, e);
			newline(ctx);
			spr(ctx, "}}");
			handle_break([]);
		case TTry(e, catchs): spr(ctx, "try ");
			gen_expr(ctx, e);
			List.iter(function (v, e): newline(ctx);
					  print(ctx, "catch[ %s : %s ]", s_ident(v.v_name), type_str(ctx, v.v_type, e.epos));
					  gen_expr(ctx, e), catchs);
		case TSwitch(e, cases, def): spr(ctx, "switch");
			gen_value(ctx, parent(e));
			spr(ctx, " {");
			newline(ctx);
			List.iter(function (el, e2): List.iter(function e: spr(ctx, "case ");
												   gen_value(ctx, e);
												   spr(ctx, ":"), el);
					  gen_block(ctx, e2);
					  print(ctx, "break");
					  newline(ctx), cases);
			switch (def) {
			case None: [];
			case Some(e): spr(ctx, "default:");
				gen_block(ctx, e);
				print(ctx, "break");
				newline(ctx);
			};
			spr(ctx, "}");
		case TCast(e1, None): var s = type_str(ctx, e.etype, e.epos);
			if ( = (s, "*")) {
				gen_expr(ctx, e1);
			} else {
				spr(ctx, "[[");
				gen_value(ctx, e1);
				print(ctx, "] as %s]", s);
			};
		case TCast(e1, Some(t)): gen_expr(ctx, Codegen.default_cast(ctx.inf.com, e1, t, e.etype, e.epos));
		};
	};

	public static function gen_block_element(ctx, e) return {
		switch (e.eexpr) {
		case TObjectDecl(fl): List.iter(function (_, e): gen_block_element(ctx, e), fl);
		case _: block_newline(ctx);
			gen_expr(ctx, e);
		};
	};

	public static function gen_block(ctx, e) return {
		newline(ctx);
		switch (e.eexpr) {
		case TBlock([]): [];
		case _: gen_expr(ctx, e);
			newline(ctx);
		};
	};

	public static function gen_value(ctx, e) return {
		function assign(e) return {
			mk(TBinop(Ast.OpAssign, mk(TLocal(switch (ctx.in_value) {
		case None: assert False;
		case Some(r): r;
			}), t_dynamic, e.epos), e), e.etype, e.epos);
		};
		function block(e) return {
			mk(TBlock(::(e, [])), e.etype, e.epos);
		};
		function value(block) return {
			var old = ctx.in_value;
			var t = type_str(ctx, e.etype, e.epos);
			var r = alloc_var(gen_local(ctx, "$r"), e.etype);
			ctx.in_value = Some(r);
			if (ctx.in_static) {
				print(ctx, "function[] : %s ", t);
			} else {
				print(ctx, "[function[$this:%s] : %s ", snd(ctx.path), t);
			};
			var b = if (block) {
				spr(ctx, "{");
				var b = open_block(ctx);
				newline(ctx);
				print(ctx, "var %s : %s", r.v_name, t);
				newline(ctx);
				b;
			} else {
				function []: [];
			};
			function []:
			if (block) {
				newline(ctx);
				print(ctx, "return %s", r.v_name);
				b([]);
				newline(ctx);
				spr(ctx, "}");
			} else {
				[];
			};
			ctx.in_value = old;
			if (ctx.in_static) {
				print(ctx, "[]");
			} else {
				print(ctx, "[%s]]", this(ctx));
			};
		};
		switch (e.eexpr) {
		case TCall({ eexpr = TLocal({ v_name = __keys__ }) }, _) | TCall({ eexpr = TLocal({ v_name = __hkeys__ }) }, _): var v =
				value(True);
			gen_expr(ctx, e);
			v([]);
		case TConst(_) | TLocal(_) | TArray(_) | TBinop(_) | TField(_) | TEnumParameter(_) | TTypeExpr(_) | TParenthesis(_) | TObjectDecl(_) | TArrayDecl(_) | TCall(_) | TNew(_) | TUnop(_) | TFunction(_)
				: gen_expr(ctx, e);
		case TMeta(_, e1): gen_value(ctx, e1);
		case TCast(e1, None): var s = type_str(ctx, e.etype, e1.epos);
			switch (s) {
			case *: gen_value(ctx, e1);
			case Function | Array | String: spr(ctx, "[[");
				gen_value(ctx, e1);
				print(ctx, "] as %s]", s);
			case _: print(ctx, "%s[", s);
				gen_value(ctx, e1);
				spr(ctx, "]");
			};
		case TCast(e1, Some(t)): gen_value(ctx, Codegen.default_cast(ctx.inf.com, e1, t, e.etype, e.epos));
		case TReturn(_) | TBreak | TContinue: unsupported(e.epos);
		case TVar(_) | TFor(_) | TWhile(_) | TThrow(_): var v = value(True);
			gen_expr(ctx, e);
			v([]);
		case TBlock([]): spr(ctx, "null");
		case TBlock(::(e, [])): gen_value(ctx, e);
		case TBlock(el): var v = value(True);
			function loop(match) return switch (match) {
			case []: spr(ctx, "return null");
			case ::(e, []): gen_expr(ctx, assign(e));
			case ::(e, l): gen_expr(ctx, e);
				newline(ctx);
				loop(l);
			};
			loop(el);
			v([]);
		case TIf(cond, e, eo): spr(ctx, "[");
			gen_value(ctx, cond);
			spr(ctx, "?");
			gen_value(ctx, e);
			spr(ctx, ":");
			switch (eo) {
			case None: spr(ctx, "null");
			case Some(e): gen_value(ctx, e);
			};
			spr(ctx, "]");
		case TSwitch(cond, cases, def): var v = value(True);
			gen_expr(ctx, mk(TSwitch(cond, List.map(function (e1, e2): (new Tuple(e1, assign(e2))), cases), switch (def) {
		case None: None;
		case Some(e): Some(assign(e));
			}), e.etype, e.epos));
			v([]);
		case TTry(b, catchs): var v = value(True);
			gen_expr(ctx, mk(TTry(block(assign(b)), List.map(function (v, e): (new Tuple(v, block(assign(e)))), catchs)), e.etype,
							 e.epos));
			v([]);
		};
	};

	public static function final(m) return {
		if (Ast.Meta.has(Ast.Meta.Final, m)) {
			"final ";
		} else {
			"";
		};
	};

	public static function generate_field(ctx, static, f) return {
		newline(ctx);
		ctx.in_static = static;
		ctx.gen_uid = 0;
		List.iter(function (m, pl, _):
		switch ((new Tuple(m, pl))) {
	case (Ast.Meta.Meta, ::((Ast.ECall((Ast.EConst(Ast.Ident(n)), _), args), _), [])): 	function mk_arg(Tuple(a, p)) return {
				switch (a) {
				case Ast.EConst(Ast.String(s)): (new Tuple(None, s));
				case Ast.EBinop(Ast.OpAssign, (Ast.EConst(Ast.Ident(n)), _), (Ast.EConst(Ast.String(s)), _)): (new Tuple(Some(n), s));
				case _: error("Invalid meta definition", p);
				};
			};
			print(ctx, "[%s", n);
			switch (args) {
			case []: [];
			case _: print(ctx, "[");
				concat(ctx, ",", function a:
				switch (mk_arg(a)) {
			case (None, s): gen_constant(ctx, snd(a), TString(s));
				case (Some(s), e): print(ctx, "%s=", s);
					gen_constant(ctx, snd(a), TString(e));
				}, args);
				print(ctx, "]");
			};
			print(ctx, "]");
		case _: [];
		}, f.cf_meta);
		var public = || (f.cf_public, || (Hashtbl.mem(ctx.get_sets, (new Tuple(f.cf_name, static))), || ( && ( = (f.cf_name, "main"), static), || ( = (f.cf_name, "resolve"), || (Ast.Meta.has(Ast.Meta.Public, f.cf_meta), switch (ctx.curclass.cl_kind) {
	case KAbstractImpl(_): True;
		case _: False;
		})))));
		var rights = ^ (if (static) {
		"static ";
	} else {
		"";
	}, if (public) {
		"public";
	} else {
		"protected";
	});
		var p = ctx.curclass.cl_pos;
		switch ((new Tuple(f.cf_expr, f.cf_kind))) {
		case (Some({ eexpr = TFunction(fd) }), Method(MethNormal | MethInline)): print(ctx, "%s%s ", rights, if (static) {
			"";
		} else {
			final(f.cf_meta);
			});
			function loop(c) return {
				switch (c.cl_super) {
				case None: [];
				case Some(c, _): if (PMap.mem(f.cf_name, c.cl_fields)) {
						spr(ctx, "override ");
					} else {
						loop(c);
					};
				};
			};
			if (!(static)) {
				loop(ctx.curclass);
			} else {
				[];
			};
			var h = gen_function_header(ctx, Some(s_ident(f.cf_name), f.cf_meta), fd, f.cf_params, p);
			gen_expr(ctx, fd.tf_expr);
			h([]);
			newline(ctx);
		case _: var is_getset = switch (f.cf_kind) {
			case Var({ v_read = AccCall }) | Var({ v_write = AccCall }): True;
			case _: False;
			};
			if (ctx.curclass.cl_interface) {
				switch (follow(f.cf_type)) {
				case TFun(args, r) if (switch (f.cf_kind) {
						case Method(MethDynamic) | Var(_): False;
							case _: True;
							}): function loop(match) return switch (match) {
					case []: f.cf_name;
					case ::((Ast.Meta.Getter, ::((Ast.EConst(Ast.String(name)), _), []), _), _): ^ ("get ", name);
					case ::((Ast.Meta.Setter, ::((Ast.EConst(Ast.String(name)), _), []), _), _): ^ ("set ", name);
					case ::(_, l): loop(l);
					};
					print(ctx, "function %s[", loop(f.cf_meta));
					concat(ctx, ",", function (arg, o, t): var tstr = type_str(ctx, t, p);
						   print(ctx, "%s : %s", arg, tstr);
					if (o) {
					print(ctx, " = %s", default_value(tstr));
					} else {
						[];
					}, args);
					print(ctx, "] : %s ", type_str(ctx, r, p));
				case _: [];
				};
			} else {
				function gen_init([]) return {
					switch (f.cf_expr) {
					case None: [];
					case Some(e): if ( || (!(static), switch (e.eexpr) {
						case TConst(_) | TFunction(_) | TTypeExpr(_): True;
							case _: False;
							})) {
							print(ctx, " = ");
							gen_value(ctx, e);
						} else {
							Codegen.ExtClass.add_static_init(ctx.curclass, f, e, e.epos);
						};
					};
				};
				if (is_getset) {
					var t = type_str(ctx, f.cf_type, p);
					var id = s_ident(f.cf_name);
					var v = switch (f.cf_kind) {
					case Var(v): v;
					case _: assert False;
					};
					switch (v.v_read) {
					case AccNormal | AccNo | AccNever: print(ctx, "%s function get %s[] : %s { return $%s; }", rights, id, t, id);
						newline(ctx);
					case AccCall: print(ctx, "%s function get %s[] : %s { return %s[]; }", rights, id, t, ^ ("get_", f.cf_name));
						newline(ctx);
					case _: [];
					};
					switch (v.v_write) {
					case AccNormal | AccNo | AccNever: print(ctx, "%s function set %s[ __v : %s ] : void { $%s = __v; }", rights, id, t, id);
						newline(ctx);
					case AccCall: print(ctx, "%s function set %s[ __v : %s ] : void { %s[__v]; }", rights, id, t, ^ ("set_", f.cf_name));
						newline(ctx);
					case _: [];
					};
					print(ctx, "%sprotected var $%s : %s", if (static) {
					"static ";
				} else {
					"";
				}, s_ident(f.cf_name), type_str(ctx, f.cf_type, p));
					gen_init([]);
				} else {
					print(ctx, "%s var %s : %s", rights, s_ident(f.cf_name), type_str(ctx, f.cf_type, p));
					gen_init([]);
				};
			};
		};
	};

	public static function define_getset(ctx, stat, c) return {
		function def(f, name) return {
			Hashtbl.add(ctx.get_sets, (new Tuple(name, stat)), f.cf_name);
		};
		function field(f) return {
			switch (f.cf_kind) {
			case Method(_): [];
			case Var(v): switch (v.v_read) {
				case AccCall: def(f, ^ ("get_", f.cf_name));
				case _: [];
				};
				switch (v.v_write) {
				case AccCall: def(f, ^ ("set_", f.cf_name));
				case _: [];
				};
			};
		};
		List.iter(field, if (stat) {
		c.cl_ordered_statics;
	} else {
		c.cl_ordered_fields;
	});
		switch (c.cl_super) {
		case Some(c, _) if (!(stat)): define_getset(ctx, stat, c);
		case _: [];
		};
	};

	public static function generate_class(ctx, c) return {
		ctx.curclass = c;
		define_getset(ctx, True, c);
		define_getset(ctx, False, c);
		ctx.local_types = List.map(snd, c.cl_params);
		var pack = open_block(ctx);
		print(ctx, "\tpublic %s%s%s %s ", final(c.cl_meta), switch (c.cl_dynamic) {
	case None: "";
	case Some(_): if (c.cl_interface) {
				"";
			} else {
				"dynamic ";
			};
		}, if (c.cl_interface) {
		"interface";
	} else {
		"class";
	}, snd(c.cl_path));
		switch (c.cl_super) {
		case None: [];
		case Some(csup, _): print(ctx, "extends %s ", s_path(ctx, True, csup.cl_path, c.cl_pos));
		};
		switch (c.cl_implements) {
		case []: [];
		case l: spr(ctx, if (c.cl_interface) {
			"extends ";
		} else {
			"implements ";
		});
			concat(ctx, ", ", function (i, _): print(ctx, "%s", s_path(ctx, True, i.cl_path, c.cl_pos)), l);
		};
		spr(ctx, "{");
		var cl = open_block(ctx);
		switch (c.cl_constructor) {
		case None: [];
		case Some(f): var f = { (f) with cf_name = snd(c.cl_path);
									cf_public = True;
									cf_kind = Method(MethNormal)
								  };
			ctx.constructor_block = True;
			generate_field(ctx, False, f);
		};
		List.iter(generate_field(ctx, False), c.cl_ordered_fields);
		List.iter(generate_field(ctx, True), c.cl_ordered_statics);
		var has_init = switch (c.cl_init) {
		case None: False;
		case Some(e): newline(ctx);
			spr(ctx, "static static_init function init[] : void");
			gen_expr(ctx, mk_block(e));
			True;
		};
		cl([]);
		newline(ctx);
		print(ctx, "}");
		pack([]);
		newline(ctx);
		print(ctx, "}");
		if (has_init) {
			newline(ctx);
			spr(ctx, "namespace static_init");
			newline(ctx);
			print(ctx, "%s.static_init::init[]", s_path(ctx, True, ctx.curclass.cl_path, Ast.null_pos));
		} else {
			[];
		};
		newline(ctx);
		if ( && (c.cl_interface, Ast.Meta.has(Ast.Meta.Custom(":hasMetadata"), c.cl_meta))) {
			var path = (new Tuple(fst(c.cl_path), ^ (snd(c.cl_path), "_HxMeta")));
			spr(ctx, Ast.s_type_path(path));
			newline(ctx);
		} else {
			[];
		};
	};

	public static function generate_main(ctx, inits) return {
		ctx.curclass = { (null_class) with cl_path = (new Tuple([], "__main__")) };
		var pack = open_block(ctx);
		print(ctx, "\timport flash.Lib");
		newline(ctx);
		print(ctx, "public class __main__ extends %s {", s_path(ctx, True, (new Tuple(::("flash", []), "Boot")), Ast.null_pos));
		var cl = open_block(ctx);
		newline(ctx);
		spr(ctx, "public function __main__[] {");
		var fl = open_block(ctx);
		newline(ctx);
		spr(ctx, "super[]");
		newline(ctx);
		spr(ctx, "flash.Lib.current = this");
		List.iter(function e: newline(ctx);
				  gen_expr(ctx, e), inits);
		fl([]);
		newline(ctx);
		print(ctx, "}");
		cl([]);
		newline(ctx);
		print(ctx, "}");
		pack([]);
		newline(ctx);
		print(ctx, "}");
		newline(ctx);
	};

	public static function generate_enum(ctx, e) return {
		ctx.local_types = List.map(snd, e.e_params);
		var pack = open_block(ctx);
		var ename = snd(e.e_path);
		print(ctx, "\tpublic final class %s extends enum {", ename);
		var cl = open_block(ctx);
		newline(ctx);
		print(ctx, "public static const __isenum : Boolean = true");
		newline(ctx);
		print(ctx, "public function %s[ t : String, index : int, p : Array = null ] : void { this.tag = t; this.index = index; this.params = p; }", ename);
		PMap.iter(function _: function c: newline(ctx);
		switch (c.ef_type) {
	case TFun(args, _): print(ctx, "public static function %s[", c.ef_name);
			concat(ctx, ", ", function (a, o, t): print(ctx, "%s : %s", s_ident(a), type_str(ctx, t, c.ef_pos));
			if (o) {
			spr(ctx, " = null");
			} else {
				[];
			}, args);
			print(ctx, "] : %s {", ename);
			print(ctx, " return new %s[\"%s\",%d,[", ename, c.ef_name, c.ef_index);
			concat(ctx, ",", function (a, _, _): spr(ctx, s_ident(a)), args);
			print(ctx, "]]; }");
		case _: print(ctx, "public static var %s : %s = new %s[\"%s\",%d]", c.ef_name, ename, ename, c.ef_name, c.ef_index);
		}, e.e_constrs);
		newline(ctx);
		switch (Codegen.build_metadata(ctx.inf.com, TEnumDecl(e))) {
		case None: [];
		case Some(e): print(ctx, "public static var __meta__ : * = ");
			gen_expr(ctx, e);
			newline(ctx);
		};
		print(ctx, "public static var __constructs__ : Array = [%s];", String.concat(",", List.map(function s: ^ ("\"", ^ (Ast.s_escape(s), "\"")), e.e_names)));
		cl([]);
		newline(ctx);
		print(ctx, "}");
		pack([]);
		newline(ctx);
		print(ctx, "}");
		newline(ctx);
	};

	public static function generate_base_enum(ctx) return {
		var pack = open_block(ctx);
		spr(ctx, "\timport flash.Boot");
		newline(ctx);
		spr(ctx, "public class enum {");
		var cl = open_block(ctx);
		newline(ctx);
		spr(ctx, "public var tag : String");
		newline(ctx);
		spr(ctx, "public var index : int");
		newline(ctx);
		spr(ctx, "public var params : Array");
		newline(ctx);
		spr(ctx, "public function toString[] : String { return flash.Boot.enum_to_string[this]; }");
		cl([]);
		newline(ctx);
		print(ctx, "}");
		pack([]);
		newline(ctx);
		print(ctx, "}");
		newline(ctx);
	};

	public static function generate(com) return {
		var infos = { () with com = com };
		generate_resources(infos);
		var ctx = init(infos, (new Tuple([], "enum")));
		generate_base_enum(ctx);
		close(ctx);
		var inits = ref([]);
		List.iter(function t:
		switch (t) {
	case TClassDecl(c): var c = switch (c.cl_path) {
			case (::(flash, []), FlashXml__): {
				(c) with cl_path = (new Tuple([], "Xml"))
			};
			case (pack, name): {
				(c) with cl_path = (new Tuple(pack, protect(name)))
			};
			};
			if (c.cl_extern) {
				switch (c.cl_init) {
				case None: [];
				case Some(e): inits.val = ::(e, inits.val);
				};
			} else {
				var ctx = init(infos, c.cl_path);
				generate_class(ctx, c);
				close(ctx);
			};
		case TEnumDecl(e): var Tuple(pack, name) = e.e_path;
			var e = { (e) with e_path = (new Tuple(pack, protect(name))) };
			if (e.e_extern) {
				[];
			} else {
				var ctx = init(infos, e.e_path);
				generate_enum(ctx, e);
				close(ctx);
			};
		case TTypeDecl(_) | TAbstractDecl(_): [];
		}, com.types);
		switch (com.main) {
		case None: [];
		case Some(e): inits.val = ::(e, inits.val);
		};
		var ctx = init(infos, (new Tuple([], "__main__")));
		generate_main(ctx, List.rev(inits.val));
		close(ctx);
	}
}
;
