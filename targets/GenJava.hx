import JData;
import Unix;
import Ast;
import Common;
import Type;
import Gencommon;
import Gencommon.SourceWriter;
import Printf;
import Option;
import ExtString;

typedef Java_lib_ctx = {
	jcom : Common.Context,
	jtparams : List<Jtypes>
};

class /*exception*/ ConversionError {

};

class Genjava {
	public static function is_boxed_type(t) return {
		switch (follow(t)) {
		case TInst({ cl_path = (::(java, ::(lang, [])), Boolean) }, []) | TInst({ cl_path = (::(java, ::(lang, [])), Double) }, []) | TInst({ cl_path = (::(java, ::(lang, [])), Integer) }, []) | TInst({ cl_path = (::(java, ::(lang, [])), Byte) }, []) | TInst({ cl_path = (::(java, ::(lang, [])), Short) }, []) | TInst({ cl_path = (::(java, ::(lang, [])), Character) }, []) | TInst({ cl_path = (::(java, ::(lang, [])), Float) }, [])
			: True;
		case TAbstract({ a_path = (::(java, ::(lang, [])), Boolean) }, []) | TAbstract({ a_path = (::(java, ::(lang, [])), Double) }, []) | TAbstract({ a_path = (::(java, ::(lang, [])), Integer) }, []) | TAbstract({ a_path = (::(java, ::(lang, [])), Byte) }, []) | TAbstract({ a_path = (::(java, ::(lang, [])), Short) }, []) | TAbstract({ a_path = (::(java, ::(lang, [])), Character) }, []) | TAbstract({ a_path = (::(java, ::(lang, [])), Float) }, [])
				: True;
		case _: False;
		};
	};

	public static function unboxed_type(gen, t, tbyte, tshort, tchar, tfloat) return {
		switch (follow(t)) {
		case TInst({ cl_path = (::(java, ::(lang, [])), Boolean) }, []): gen.gcon.basic.tbool;
		case TInst({ cl_path = (::(java, ::(lang, [])), Double) }, []): gen.gcon.basic.tfloat;
		case TInst({ cl_path = (::(java, ::(lang, [])), Integer) }, []): gen.gcon.basic.tint;
		case TInst({ cl_path = (::(java, ::(lang, [])), Byte) }, []): tbyte;
		case TInst({ cl_path = (::(java, ::(lang, [])), Short) }, []): tshort;
		case TInst({ cl_path = (::(java, ::(lang, [])), Character) }, []): tchar;
		case TInst({ cl_path = (::(java, ::(lang, [])), Float) }, []): tfloat;
		case TAbstract({ a_path = (::(java, ::(lang, [])), Boolean) }, []): gen.gcon.basic.tbool;
		case TAbstract({ a_path = (::(java, ::(lang, [])), Double) }, []): gen.gcon.basic.tfloat;
		case TAbstract({ a_path = (::(java, ::(lang, [])), Integer) }, []): gen.gcon.basic.tint;
		case TAbstract({ a_path = (::(java, ::(lang, [])), Byte) }, []): tbyte;
		case TAbstract({ a_path = (::(java, ::(lang, [])), Short) }, []): tshort;
		case TAbstract({ a_path = (::(java, ::(lang, [])), Character) }, []): tchar;
		case TAbstract({ a_path = (::(java, ::(lang, [])), Float) }, []): tfloat;
		case _: assert False;
		};
	};

	public static function t_has_type_param(t) return {
		switch (follow(t)) {
		case TInst({ cl_kind = KTypeParameter(_) }, []): True;
		case TEnum(_, params) | TAbstract(_, params) | TInst(_, params): List.exists(t_has_type_param, params);
		case TFun(f, ret): || (t_has_type_param(ret), List.exists(function (_, _, t): t_has_type_param(t), f));
		case _: False;
		};
	};

	public static function is_type_param(t) return {
		switch (follow(t)) {
		case TInst({ cl_kind = KTypeParameter(_) }, _): True;
		case _: False;
		};
	};

	public static function t_has_type_param_shallow(last, t) return {
		switch (follow(t)) {
		case TInst({ cl_kind = KTypeParameter(_) }, []): True;
		case TEnum(_, params) | TAbstract(_, params) | TInst(_, params) if (!(last)): List.exists(t_has_type_param_shallow(True),
					params);
		case TFun(f, ret) if (!(last)):
			|| (t_has_type_param_shallow(True, ret), List.exists(function (_, _, t): t_has_type_param_shallow(True, t), f));
		case _: False;
		};
	};

	public static function replace_type_param(t) return {
		switch (follow(t)) {
		case TInst({ cl_kind = KTypeParameter(_) }, []): t_dynamic;
		case TEnum(e, params): TEnum(e, List.map(replace_type_param, params));
		case TAbstract(a, params): TAbstract(a, List.map(replace_type_param, params));
		case TInst(cl, params): TInst(cl, List.map(replace_type_param, params));
		case _: t;
		};
	};

	public static function is_java_basic_type(t) return {
		switch (follow(t)) {
		case TInst({ cl_path = (::(haxe, []), Int32) }, []) | TInst({ cl_path = (::(haxe, []), Int64) }, []) | TAbstract({ a_path = ([], Single) }, []) | TAbstract({ a_path = (::(java, []), Int8 | Int16 | Char16 | Int64) }, []) | TAbstract({ a_path = ([], Int) }, []) | TAbstract({ a_path = ([], Float) }, []) | TAbstract({ a_path = ([], Bool) }, [])
			: True;
		case _: False;
		};
	};

	public static function is_bool(t) return {
		switch (follow(t)) {
		case TAbstract({ a_path = ([], Bool) }, []): True;
		case _: False;
		};
	};

	public static function like_bool(t) return {
		switch (follow(t)) {
		case TAbstract({ a_path = ([], Bool) }, []) | TAbstract({ a_path = (::(java, ::(lang, [])), Boolean) }, []) | TInst({ cl_path = (::(java, ::(lang, [])), Boolean) }, [])
			: True;
		case _: False;
		};
	};

	public static function is_int_float(gen, t) return {
		switch (follow(gen.greal_type(t))) {
		case TInst({ cl_path = (::(haxe, []), Int32) }, []) | TAbstract({ a_path = ([], Int) }, []) | TAbstract({ a_path = ([], Float) }, [])
			: True;
		case TAbstract(_) = t if (&&(like_float(t), !(like_i64(t)))): True;
		case _: False;
		};
	};

	public static var parse_explicit_iface = var regex = Str.regexp("\\.");
	function parse_explicit_iface(str) return {
		var split = Str.split(regex, str);
		function get_iface(split, pack) return {
			switch (split) {
			case ::(clname, ::(fn_name, [])): (new Tuple(fn_name, (new Tuple(List.rev(pack), clname))));
			case ::(pack_piece, tl): get_iface(tl, ::(pack_piece, pack));
			case _: assert False;
			};
		};
		get_iface(split, []);
	};
	parse_explicit_iface;

	public static function is_string(t) return {
		switch (follow(t)) {
		case TInst({ cl_path = ([], String) }, []): True;
		case _: False;
		};
	};

	public static function is_cl(t) return {
		switch (follow(t)) {
		case TInst({ cl_path = (::(java, ::(lang, [])), Class) }, _) | TAbstract({ a_path = ([], Class | Enum) }, _): True;
		case TAnon(a) if (is_some(anon_class(t))): True;
		case _: False;
		};
	};

	public static function is_checked_exc(cl) return {
		switch (cl.cl_path) {
		case (::(java, ::(lang, [])), RuntimeException): False;
		case (::(java, ::(lang, [])), Throwable): True;
		case _: switch (cl.cl_super) {
			case None: False;
			case Some(c, _): is_checked_exc(c);
			};
		};
	};

	public static function cls_any_super(cl, supers) return {
		|| (PMap.mem(cl.cl_path, supers), switch (cl.cl_super) {
	case None: False;
	case Some(c, _): cls_any_super(c, supers);
		});
	};

	public static function handle_throws(gen, cf) return {
		List.iter(handle_throws(gen), cf.cf_overloads);
		switch (cf.cf_expr) {
		case Some({ eexpr = TFunction(tf) } = e): 	function collect_throws(acc) return {
			case ::((Meta.Throws, ::((Ast.EConst(Ast.String(path)), _), []), _), meta): try {
					collect_throws(::(get_cl(get_type(gen, parse_path(path))), acc), meta);
				} catch (e: Not_found | TypeNotFound(_)) {
					collect_throws(acc, meta);
				};
			case []: acc;
			case ::(_, meta): collect_throws(acc, meta);
			};
			var cf_throws = collect_throws([], cf.cf_meta);
			var throws = ref(List.fold_left(function map: function cl: PMap.add(cl.cl_path, cl, map), PMap.empty, cf_throws));
			function iter(e) return {
				switch (e.eexpr) {
				case TTry(etry, ecatches): var old = throws.val;
					var needs_check_block = ref(True);
					List.iter(function (v, e): Type.iter(iter, e);
					switch (follow(run_follow(gen, v.v_type))) {
				case TInst({ cl_path = (::(java, ::(lang, [])), Throwable) }, _) | TDynamic(_): needs_check_block.val = False;
					case TInst(c, _) if (is_checked_exc(c)): throws.val = PMap.add(c.cl_path, c, throws.val);
					case _: [];
					}, ecatches);
					if (needs_check_block.val) {
						Type.iter(iter, etry);
					} else {
						[];
					};
					throws.val = old;
				case TField(e, FInstance(_, _, f) | FStatic(_, f) | FClosure(_, f)): var tdefs = collect_throws([], f.cf_meta);
					if ( && (<>(tdefs, []), !(List.for_all(function c: cls_any_super(c, throws.val), tdefs)))) {
						raise(Exit);
					} else {
						[];
					};
					Type.iter(iter, e);
				case TThrow(e): switch (follow(run_follow(gen, e.etype))) {
					case TInst(c, _) if (&&(is_checked_exc(c), !(cls_any_super(c, throws.val)))): raise(Exit);
					case _: iter(e);
					};
				case _: Type.iter(iter, e);
				};
			};
			try {
				Type.iter(iter, e);
			} catch (e: Exit) {
				var throwable = get_cl(get_type(gen, (new Tuple(::("java", ::("lang", [])), "Throwable"))));
				var catch_var = alloc_var("typedException", TInst(throwable, []));
				var rethrow = mk_local(catch_var, e.epos);
				var hx_exception = get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "HaxeException"))));
				var wrap_static = mk_static_field_access(hx_exception, "wrap", TFun(::((new Tuple("obj", False, t_dynamic)), []),
								  t_dynamic), rethrow.epos);
				var wrapped = { (rethrow) with eexpr = TThrow({ (rethrow) with eexpr = TCall(wrap_static, ::(rethrow, [])) }) };
				function map_throws(cl) return {
					var var = alloc_var("typedException", TInst(cl, List.map(function _: t_dynamic, cl.cl_params)));
					(new Tuple(var, { (tf.tf_expr) with eexpr = TThrow(mk_local(var, e.epos)) }));
				};
				cf.cf_expr = Some({ (e) with eexpr = TFunction({ (tf) with tf_expr = mk_block({ (tf.tf_expr) with eexpr = TTry(tf.tf_expr, @(List.map(map_throws, cf_throws), ::((new Tuple(catch_var, wrapped)), []))) }) }) });
			};
		case _: [];
		};
	};

	public static var connecting_string = "?";

	public static var default_package = "java";

	public static var strict_mode = ref(False);

	public static var reserved = var res = Hashtbl.create(120);
	List.iter(function lst: Hashtbl.add(res, lst, ^ ("_", lst)), ::("abstract", ::("assert", ::("boolean", ::("break",
			  ::("byte", ::("case", ::("catch", ::("char", ::("class", ::("const", ::("continue", ::("default", ::("do", ::("double",
									   ::("else", ::("enum", ::("extends", ::("final", ::("false", ::("finally", ::("float", ::("for", ::("goto", ::("if",
											   ::("implements", ::("import", ::("instanceof", ::("int", ::("interface", ::("long", ::("native", ::("new", ::("null",
													   ::("package", ::("private", ::("protected", ::("public", ::("return", ::("short", ::("static", ::("strictfp", ::("super",
															   ::("switch", ::("synchronized", ::("this", ::("throw", ::("throws", ::("transient", ::("true", ::("try", ::("void",
																	   ::("volatile", ::("while", []))))))))))))))))))))))))))))))))))))))))))))))))))))));
	res;

	public static var dynamic_anon = TAnon({ () with a_fields = PMap.empty;
										   a_status = ref(Closed)
										   });

	public static function get_class_modifiers(meta, cl_type, cl_access, cl_modifiers) return {
		switch (meta) {
		case []: (new Tuple(cl_type, cl_access, cl_modifiers));
		case ::((Meta.Protected, [], _), meta): get_class_modifiers(meta, cl_type, "protected", cl_modifiers);
		case ::((Meta.Internal, [], _), meta): get_class_modifiers(meta, cl_type, "", cl_modifiers);
		case ::((Meta.Final, [], _), meta): get_class_modifiers(meta, cl_type, cl_access, ::("final", cl_modifiers));
		case ::(_, meta): get_class_modifiers(meta, cl_type, cl_access, cl_modifiers);
		};
	};

	public static function get_fun_modifiers(meta, access, modifiers) return {
		switch (meta) {
		case []: (new Tuple(access, modifiers));
		case ::((Meta.Protected, [], _), meta): get_fun_modifiers(meta, "protected", modifiers);
		case ::((Meta.Internal, [], _), meta): get_fun_modifiers(meta, "", modifiers);
		case ::((Meta.ReadOnly, [], _), meta): get_fun_modifiers(meta, access, ::("final", modifiers));
		case ::((Meta.Volatile, [], _), meta): get_fun_modifiers(meta, access, ::("volatile", modifiers));
		case ::((Meta.Transient, [], _), meta): get_fun_modifiers(meta, access, ::("transient", modifiers));
		case ::((Meta.Native, [], _), meta): get_fun_modifiers(meta, access, ::("native", modifiers));
		case ::(_, meta): get_fun_modifiers(meta, access, modifiers);
		};
	};

	public static function configure(gen) return {
		var native_arr_cl = get_cl(get_type(gen, (new Tuple(::("java", []), "NativeArray"))));
		gen.gclasses.nativearray = function t: TInst(native_arr_cl, ::(t, []));
	gen.gclasses.nativearray_type = function case TInst(_, ::(t, [])): t;
	case _: assert False;
		gen.gclasses.nativearray_len = function e: function p: mk_field_access(gen, e, "length", p);
		var basic = gen.gcon.basic;
		var fn_cl = get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "Function"))));
		var runtime_cl = get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "Runtime"))));
		var nulltdef = get_tdef(get_type(gen, (new Tuple([], "Null"))));
		var ti64 = switch (get_type(gen, (new Tuple(::("java", []), "Int64")))) {
		case TAbstractDecl(a): TAbstract(a, []);
		case _: assert False;
		};
		function has_tdynamic(params) return {
			List.exists(function e:
			switch (run_follow(gen, e)) {
		case TDynamic(_): True;
			case _: False;
			}, params);
		};
		function change_param_type(md, params) return {
			switch (md) {
			case TClassDecl({ cl_path = (::(java, []), NativeArray) }): params;
			case TAbstractDecl({ a_path = ([], Class | Enum) }) | TClassDecl({ cl_path = (::(java, ::(lang, [])), Class | Enum) }):
				List.map(function _: t_dynamic, params);
			case _: switch (params) {
				case []: [];
				case _: if (has_tdynamic(params)) {
						List.map(function _: t_dynamic, params);
					} else {
						List.map(function t: var f_t = gen.gfollowrun_f(t);
						switch (f_t) {
					case TAbstract({ a_path = ([], Bool) }, []) | TAbstract({ a_path = ([], Float) }, []) | TInst({ cl_path = (::(haxe, []), Int32) }, []) | TInst({ cl_path = (::(haxe, []), Int64) }, []) | TAbstract({ a_path = ([], Int) }, []) | TType({ t_path = (::(java, []), Int64) }, []) | TAbstract({ a_path = (::(java, []), Int64) }, []) | TType({ t_path = (::(java, []), Int8) }, []) | TAbstract({ a_path = (::(java, []), Int8) }, []) | TType({ t_path = (::(java, []), Int16) }, []) | TAbstract({ a_path = (::(java, []), Int16) }, []) | TType({ t_path = (::(java, []), Char16) }, []) | TAbstract({ a_path = (::(java, []), Char16) }, []) | TType({ t_path = ([], Single) }, []) | TAbstract({ a_path = ([], Single) }, [])
								: TType(nulltdef, ::(f_t, []));
						case TInst(cl, ::(_, _) = p) if (<>(cl.cl_path, (new Tuple(::("java", []), "NativeArray")))): TInst(cl, p);
						case TEnum(e, ::(_, _) = p): TEnum(e, List.map(function _: t_dynamic, p));
						case _: t;
						}, params);
					};
				};
			};
		};
		function change_clname(name) return {
		String.map(function case '$': '.';
			case c: c, name);
		};
		function change_id(name) return {
			try {
				Hashtbl.find(reserved, name);
			} catch (e: Not_found) {
				name;
			};
		};
		function change_ns(ns) return {
			switch (ns) {
			case []: ::("haxe", ::("root", []));
			case _: List.map(change_id, ns);
			};
		};
		var change_field = change_id;
		function write_id(w, name) return {
			write(w, change_id(name));
		};
		function write_field(w, name) return {
			write(w, change_field(name));
		};
		gen.gfollowadd(name = "follow_basic", function t:
		switch (t) {
	case TAbstract({ a_path = ([], Bool) }, []) | TAbstract({ a_path = ([], Void) }, []) | TAbstract({ a_path = ([], Float) }, []) | TAbstract({ a_path = ([], Int) }, []) | TInst({ cl_path = (::(haxe, []), Int32) }, []) | TInst({ cl_path = (::(haxe, []), Int64) }, []) | TType({ t_path = (::(java, []), Int64) }, []) | TAbstract({ a_path = (::(java, []), Int64) }, []) | TType({ t_path = (::(java, []), Int8) }, []) | TAbstract({ a_path = (::(java, []), Int8) }, []) | TType({ t_path = (::(java, []), Int16) }, []) | TAbstract({ a_path = (::(java, []), Int16) }, []) | TType({ t_path = (::(java, []), Char16) }, []) | TAbstract({ a_path = (::(java, []), Char16) }, []) | TType({ t_path = ([], Single) }, []) | TAbstract({ a_path = ([], Single) }, [])
				: Some(t);
		case TType({ t_path = ([], Null) } = tdef, ::(t2, [])): Some(TType(tdef, ::(gen.gfollowrun_f(t2), [])));
		case TAbstract(a, pl) if (!(Meta.has(Meta.CoreType, a.a_meta))): Some(gen.gfollowrun_f(Abstract.get_underlying_type(a,
					pl)));
		case TAbstract({ a_path = ([], EnumValue) }, _) | TInst({ cl_path = ([], EnumValue) }, _): Some(t_dynamic);
		case _: None;
		});
		function change_path(path) return {
			(new Tuple(change_ns(fst(path)), change_clname(snd(path))));
		};
		function path_s(path, meta) return {
			try {
				switch (Meta.get(Meta.JavaCanonical, meta)) {
				case (Meta.JavaCanonical, ::((EConst(String(pack)), _), ::((EConst(String(name)), _), [])), _): if ( = (pack, "")) {
						name;
					} else {
						^ (pack, ^ (".", name));
					};
				case _: raise(Not_found);
				};
			} catch (e: Not_found) {
				switch (path) {
					(ns, clname): path_s((new Tuple(change_ns(ns), change_clname(clname))));
				};
			};
		};
		var cl_cl = get_cl(get_type(gen, (new Tuple(::("java", ::("lang", [])), "Class"))));
		function real_type(t) return {
			var t = gen.gfollowrun_f(t);
			switch (t) {
			case TAbstract(a, pl) if (!(Meta.has(Meta.CoreType, a.a_meta))): real_type(Abstract.get_underlying_type(a, pl));
			case TInst({ cl_path = (::(haxe, []), Int32) }, []): gen.gcon.basic.tint;
			case TInst({ cl_path = (::(haxe, []), Int64) }, []): ti64;
			case TAbstract({ a_path = ([], Class) }, p) | TAbstract({ a_path = ([], Enum) }, p) | TInst({ cl_path = ([], Class) }, p) | TInst({ cl_path = ([], Enum) }, p)
					: TInst(cl_cl, ::(t_dynamic, []));
			case TEnum(e, params): TEnum(e, List.map(function _: t_dynamic, params));
			case TInst(c, params) if (Meta.has(Meta.Enum, c.cl_meta)): TInst(c, List.map(function _: t_dynamic, params));
			case TInst({ cl_kind = KExpr(_) }, _): t_dynamic;
			case TInst(_): t;
			case TType({ t_path = ([], Null) }, ::(t, [])) if (is_java_basic_type(gen.gfollowrun_f(t))): t_dynamic;
			case TType({ t_path = ([], Null) }, ::(t, [])):
				switch (follow(t)) {
				case TInst({ cl_kind = KTypeParameter(_) }, []): t_dynamic;
				case _: real_type(t);
				};
			case TType(_) | TAbstract(_): t;
			case TAnon(anon): switch (anon.a_status.val) {
				case Statics(_) | EnumStatics(_) | AbstractStatics(_): t;
				case _: t_dynamic;
				};
			case TFun(_): TInst(fn_cl, []);
			case _: t_dynamic;
			};
		};
		var scope = ref(PMap.empty);
		var imports = ref([]);
		function clear_scope([]) return {
			scope.val = PMap.empty;
			imports.val = [];
		};
		function add_scope(name) return {
			scope.val = PMap.add(name, [], scope.val);
		};
		function add_import(pos, path, meta) return {
			var name = snd(path);
			function loop(match) return switch (match) {
			case ::((pack, n), _) if (=(name, n)):
				if (<>(path, (new Tuple(pack, n)))) {
					gen.gcon.error( ^ ("This expression cannot be generated because ", ^ (path_s(path, meta),
					" is shadowed by the current scope and ")), pos);
				} else {
					[];
				};
			case ::(_, tl): loop(tl);
			case []: imports.val = ::(path, imports.val);
			};
			loop(imports.val);
		};
		function path_s_import(pos, path, meta) return {
			switch (path) {
			case ([], name) if (PMap.mem(name, scope.val)): gen.gcon.error( ^ ("This expression cannot be generated because ", ^ (name,
				" is shadowed by the current scope")), pos);
				name;
			case (::(pack1, _), name) if (PMap.mem(pack1, scope.val)): add_import(pos, path, meta);
				if (PMap.mem(name, scope.val)) {
					gen.gcon.error( ^ ("This expression cannot be generated because ", ^ (pack1, ^ (" and ", ^ (name,
					" are both shadowed by the current scope")))), pos);
				} else {
					[];
				};
				name;
			case _: path_s(path, meta);
			};
		};
		function is_dynamic(t) return {
			switch (real_type(t)) {
			case TMono(_) | TDynamic(_) | TInst({ cl_kind = KTypeParameter(_) }, _): True;
			case TAnon(anon): switch (anon.a_status.val) {
				case EnumStatics(_) | Statics(_) | AbstractStatics(_): False;
				case _: True;
				};
			case _: False;
			};
		};
		function t_s(pos, t) return {
			switch (real_type(t)) {
			case TAbstract({ a_path = ([], Bool) }, []): "boolean";
			case TAbstract({ a_path = ([], Void) }, []): path_s_import(pos, (new Tuple(::("java", ::("lang", [])), "Object")), []);
			case TAbstract({ a_path = ([], Float) }, []): "double";
			case TAbstract({ a_path = ([], Int) }, []): "int";
			case TType({ t_path = (::(java, []), Int64) }, []) | TAbstract({ a_path = (::(java, []), Int64) }, []): "long";
			case TType({ t_path = (::(java, []), Int8) }, []) | TAbstract({ a_path = (::(java, []), Int8) }, []): "byte";
			case TType({ t_path = (::(java, []), Int16) }, []) | TAbstract({ a_path = (::(java, []), Int16) }, []): "short";
			case TType({ t_path = (::(java, []), Char16) }, []) | TAbstract({ a_path = (::(java, []), Char16) }, []): "char";
			case TType({ t_path = ([], Single) }, []) | TAbstract({ a_path = ([], Single) }, []): "float";
			case TInst({ cl_path = (::(haxe, []), Int32) }, []) | TAbstract({ a_path = (::(haxe, []), Int32) }, []): "int";
			case TInst({ cl_path = (::(haxe, []), Int64) }, []) | TAbstract({ a_path = (::(haxe, []), Int64) }, []): "long";
			case TInst({ cl_path = (::(java, []), NativeArray) }, ::(param, [])): 	function check_t_s(t) return {
					switch (real_type(t)) {
					case TInst({ cl_path = (::(java, []), NativeArray) }, ::(param, [])): ^ (check_t_s(param), "[]");
					case _: t_s(pos, run_follow(gen, t));
					};
				};
				^ (check_t_s(param), "[]");
			case TInst({ cl_kind = KTypeParameter(_); cl_path = p }, []): snd(p);
			case TAbstract({ a_path = ([], Dynamic) }, []): path_s_import(pos, (new Tuple(::("java", ::("lang", [])), "Object")), []);
			case TMono(r): switch (r.val) {
				case None: "java.lang.Object";
				case Some(t): t_s(pos, run_follow(gen, t));
				};
			case TInst({ cl_path = ([], String) }, []): path_s_import(pos, (new Tuple(::("java", ::("lang", [])), "String")), []);
			case TAbstract({ a_path = ([], Class) }, ::(p, [])) | TAbstract({ a_path = ([], Enum) }, ::(p, [])) | TInst({ cl_path = ([], Class) }, ::(p, [])) | TInst({ cl_path = ([], Enum) }, ::(p, []))
					: path_param_s(pos, TClassDecl(cl_cl), (new Tuple(::("java", ::("lang", [])), "Class")), ::(p, []), []);
			case TAbstract({ a_path = ([], Class) }, _) | TAbstract({ a_path = ([], Enum) }, _) | TInst({ cl_path = ([], Class) }, _) | TInst({ cl_path = ([], Enum) }, _)
					: path_s_import(pos, (new Tuple(::("java", ::("lang", [])), "Class")), []);
			case TEnum({ e_path = p; e_meta = meta }, _): path_s_import(pos, p, meta);
			case TInst({ cl_path = p; cl_meta = meta } = cl, _) if (Meta.has(Meta.Enum, cl.cl_meta)): path_s_import(pos, p, meta);
			case TInst({ cl_path = p; cl_meta = meta } = cl, params): path_param_s(pos, TClassDecl(cl), p, params, meta);
			case TType({ t_path = p; t_meta = meta } = t, params): path_param_s(pos, TTypeDecl(t), p, params, meta);
			case TAnon(anon): switch (anon.a_status.val) {
				case Statics(_) | EnumStatics(_) | AbstractStatics(_): path_s_import(pos, (new Tuple(::("java", ::("lang", [])),
							"Class")), []);
				case _: path_s_import(pos, (new Tuple(::("java", ::("lang", [])), "Object")), []);
				};
			case TDynamic(_): path_s_import(pos, (new Tuple(::("java", ::("lang", [])), "Object")), []);
			case _: if (strict_mode.val) {
					trace( ^ ("[ !TypeError ", ^ (Type.s_type(Type.print_context([]), t), " ]")));
					assert False;
				} else {
					^ ("[ !TypeError ", ^ (Type.s_type(Type.print_context([]), t), " ]"));
				};
			};
		};
		function param_t_s(pos, t) return {
			switch (run_follow(gen, t)) {
			case TAbstract({ a_path = ([], Bool) }, []): path_s_import(pos, (new Tuple(::("java", ::("lang", [])), "Boolean")), []);
			case TAbstract({ a_path = ([], Float) }, []): path_s_import(pos, (new Tuple(::("java", ::("lang", [])), "Double")), []);
			case TAbstract({ a_path = ([], Int) }, []): path_s_import(pos, (new Tuple(::("java", ::("lang", [])), "Integer")), []);
			case TType({ t_path = (::(java, []), Int64) }, []) | TAbstract({ a_path = (::(java, []), Int64) }, []): path_s_import(pos,
						(new Tuple(::("java", ::("lang", [])), "Long")), []);
			case TInst({ cl_path = (::(haxe, []), Int64) }, []) | TAbstract({ a_path = (::(haxe, []), Int64) }, []): path_s_import(pos,
						(new Tuple(::("java", ::("lang", [])), "Long")), []);
			case TInst({ cl_path = (::(haxe, []), Int32) }, []) | TAbstract({ a_path = (::(haxe, []), Int32) }, []): path_s_import(pos,
						(new Tuple(::("java", ::("lang", [])), "Integer")), []);
			case TType({ t_path = (::(java, []), Int8) }, []) | TAbstract({ a_path = (::(java, []), Int8) }, []): path_s_import(pos,
						(new Tuple(::("java", ::("lang", [])), "Byte")), []);
			case TType({ t_path = (::(java, []), Int16) }, []) | TAbstract({ a_path = (::(java, []), Int16) }, []): path_s_import(pos,
						(new Tuple(::("java", ::("lang", [])), "Short")), []);
			case TType({ t_path = (::(java, []), Char16) }, []) | TAbstract({ a_path = (::(java, []), Char16) }, []): path_s_import(
					pos, (new Tuple(::("java", ::("lang", [])), "Character")), []);
			case TType({ t_path = ([], Single) }, []) | TAbstract({ a_path = ([], Single) }, []): path_s_import(pos,
						(new Tuple(::("java", ::("lang", [])), "Float")), []);
			case TDynamic(_): "?";
			case TInst(cl, params): t_s(pos, TInst(cl, change_param_type(TClassDecl(cl), params)));
			case TType(cl, params): t_s(pos, TType(cl, change_param_type(TTypeDecl(cl), params)));
			case TEnum(e, params): t_s(pos, TEnum(e, change_param_type(TEnumDecl(e), params)));
			case _: t_s(pos, t);
			};
		};
		function path_param_s(pos, md, path, params, meta) return {
			switch (params) {
			case []: path_s_import(pos, path, meta);
			case _ if (has_tdynamic(change_param_type(md, params))): path_s_import(pos, path, meta);
			case _: sprintf("%s<%s>", path_s_import(pos, path, meta), String.concat(", ", List.map(function t: param_t_s(pos, t),
				change_param_type(md, params))));
			};
		};
		function rett_s(pos, t) return {
			switch (t) {
			case TAbstract({ a_path = ([], Void) }, []): "void";
			case _: t_s(pos, t);
			};
		};
		function high_surrogate(c) return {
			+(lsr(c, 10), 0xD7C0);
		};
		function low_surrogate(c) return {
			lor(land(c, 0x3FF), 0xDC00);
		};
		function escape(ichar, b) return {
			switch (ichar) {
			case 92: Buffer.add_string(b, "\\\\");
			case 39: Buffer.add_string(b, "\\\'");
			case 34: Buffer.add_string(b, "\\\"");
			case 13: Buffer.add_string(b, "\\r");
			case 10: Buffer.add_string(b, "\\n");
			case 9: Buffer.add_string(b, "\\t");
			case c if (||(<(c, 32), &&(>=(c, 127), <=(c, 0xFFFF)))): Buffer.add_string(b, Printf.sprintf("\\u%.4x", c));
			case c if (>(c, 0xFFFF)): Buffer.add_string(b, Printf.sprintf("\\u%.4x\\u%.4x", high_surrogate(c), low_surrogate(c)));
			case c: Buffer.add_char(b, Char.chr(c));
			};
		};
		function escape(s) return {
			var b = Buffer.create(0);
			try {
				UTF8.validate(s);
				UTF8.iter(function c: escape(UChar.code(c), b), s);
			} catch (e: UTF8.Malformed_code) {
				String.iter(function c: escape(Char.code(c), b), s);
			};
			Buffer.contents(b);
		};
		function has_semicolon(e) return {
			switch (e.eexpr) {
			case TLocal({ v_name = __fallback__ }) | TCall({ eexpr = TLocal({ v_name = __label__ }) }, ::({ eexpr = TConst(TInt(_)) }, []))
				: False;
			case TCall({ eexpr = TLocal({ v_name = __lock__ }) }, _): False;
			case TBlock(_) | TFor(_) | TSwitch(_) | TTry(_) | TIf(_): False;
			case TWhile(_, _, flag) if (=(flag, Ast.NormalWhile)): False;
			case _: True;
			};
		};
		var in_value = ref(False);
		function md_s(pos, md) return {
			var md = follow_module(gen.gfollowrun_f, md);
			switch (md) {
			case TClassDecl(cl): t_s(pos, TInst(cl, []));
			case TEnumDecl(e): t_s(pos, TEnum(e, []));
			case TTypeDecl(t): t_s(pos, TType(t, []));
			case TAbstractDecl(a): t_s(pos, TAbstract(a, []));
			};
		};
		function transform_nativearray_t(t) return {
			switch (real_type(t)) {
			case TInst({ cl_path = (::(java, []), NativeArray) } = narr, ::(t, [])): TInst(narr, ::(transform_nativearray_t(t), []));
			case TInst(cl, params): TInst(cl, List.map(function _: t_dynamic, params));
			case TEnum(e, params): TEnum(e, List.map(function _: t_dynamic, params));
			case TType(t, params): TType(t, List.map(function _: t_dynamic, params));
			case _: t;
			};
		};
		function extract_tparams(params, el) return {
			switch (el) {
			case ::( {
					eexpr = TLocal({ v_name = $type_param })
				} = tp, tl): extract_tparams(::(tp.etype, params), tl);
			case _: (new Tuple(params, el));
			};
		};
		var line_directive = if (Common.defined(gen.gcon, Define.RealPosition)) {
			function w: function p: [];
		} else {
			function w: function p: var cur_line = Lexer.get_error_line(p);
			var file = Common.get_full_path(p.pfile);
			print(w, "//line %d \"%s\"", cur_line, Ast.s_escape(file));
			newline(w);
		};
		function extract_statements(expr) return {
			var ret = ref([]);
			function loop(expr) return {
				switch (expr.eexpr) {
				case TCall({ eexpr = TLocal({ v_name = __is__ | __typeof__ | __array__ }) }, el): List.iter(loop, el);
				case TNew({ cl_path = (::(java, []), NativeArray) }, params, ::(size, [])): [];
				case TUnop(Ast.Increment, _, _) | TUnop(Ast.Decrement, _, _) | TBinop(Ast.OpAssign, _, _) | TBinop(Ast.OpAssignOp(_), _, _) | TLocal({ v_name = __fallback__ }) | TLocal({ v_name = __sbreak__ })
						: ret.val = ::(expr, ret.val);
				case TConst(_) | TLocal(_) | TArray(_) | TBinop(_) | TField(_) | TEnumParameter(_) | TTypeExpr(_) | TObjectDecl(_) | TArrayDecl(_) | TCast(_) | TMeta(_) | TParenthesis(_) | TUnop(_)
						: Type.iter(loop, expr);
				case TFunction(_): [];
				case _: ret.val = ::(expr, ret.val);
				};
			};
			loop(expr);
			List.rev(ret.val);
		};
		function expr_s(w, e) return {
			in_value.val = False;
			function expr_s(w, e) return {
				var was_in_value = in_value.val;
				in_value.val = True;
				switch (e.eexpr) {
				case TConst(c): switch (c) {
					case TInt(i32): print(w, "%ld", i32);
						switch (real_type(e.etype)) {
						case TType({ t_path = (::(java, []), Int64) }, []): write(w, "L");
						case _: [];
						};
					case TFloat(s): write(w, s);
						if (!( || (String.contains(s, '.'), || (String.contains(s, 'e'), String.contains(s, 'E'))))) {
							write(w, ".0");
						} else {
							[];
						};
						switch (real_type(e.etype)) {
						case TType({ t_path = ([], Single) }, []): write(w, "f");
						case _: [];
						};
					case TString(s): print(w, "\"%s\"", escape(s));
					case TBool(b): write(w, if (b) {
						"true";
					} else {
						"false";
					});
					case TNull: switch (real_type(e.etype)) {
						case TAbstract({ a_path = (::(java, []), Int64) }, []) | TInst({ cl_path = (::(haxe, []), Int64) }, []): write(w, "0L");
						case TInst({ cl_path = (::(haxe, []), Int32) }, []) | TAbstract({ a_path = ([], Int) }, []): expr_s(w, { (e) with eexpr = TConst(TInt(Int32.zero)) });
						case TAbstract({ a_path = ([], Float) }, []): expr_s(w, { (e) with eexpr = TConst(TFloat("0.0")) });
						case TAbstract({ a_path = ([], Bool) }, []): write(w, "false");
						case TAbstract(_) if (like_int(e.etype)): expr_s(w, mk_cast(e.etype, { (e) with eexpr = TConst(TInt(Int32.zero)) }));
						case TAbstract(_) if (like_float(e.etype)): expr_s(w, mk_cast(e.etype, { (e) with eexpr = TConst(TFloat("0.0")) }));
						case t: write(w, "null");
						};
					case TThis: write(w, "this");
					case TSuper: write(w, "super");
					};
				case TLocal({ v_name = __fallback__ }): [];
				case TLocal({ v_name = __sbreak__ }): write(w, "break");
				case TLocal({ v_name = __undefined__ }): write(w, t_s(e.epos, TInst(runtime_cl, List.map(function _: t_dynamic,
							runtime_cl.cl_params))));
					write(w, ".undefined");
				case TLocal(var): write_id(w, var.v_name);
				case TField(_, FEnum(en, ef)): var s = ef.ef_name;
					print(w, "%s.", path_s_import(e.epos, en.e_path, en.e_meta));
					write_field(w, s);
				case TArray(e1, e2): expr_s(w, e1);
					write(w, "[");
					expr_s(w, e2);
					write(w, "]");
				case TBinop(Ast.OpAssign = op, e1, e2) | TBinop(Ast.OpAssignOp(_) = op, e1, e2): expr_s(w, e1);
					write(w, ^ (" ", ^ (Ast.s_binop(op), " ")));
					expr_s(w, e2);
				case TBinop(op, e1, e2): write(w, "[ ");
					expr_s(w, e1);
					write(w, ^ (" ", ^ (Ast.s_binop(op), " ")));
					expr_s(w, e2);
					write(w, " ]");
				case TField(e, FStatic(_, cf)) if (Meta.has(Meta.Native, cf.cf_meta)): 	function loop(meta) return {
						switch (meta) {
						case ::((Meta.Native, ::((EConst(String(s)), _), []), _), _): expr_s(w, e);
							write(w, ".");
							write_field(w, s);
						case ::(_, tl): loop(tl);
						case []: expr_s(w, e);
							write(w, ".");
							write_field(w, cf.cf_name);
						};
					};
					loop(cf.cf_meta);
				case TField(e, s): expr_s(w, e);
					write(w, ".");
					write_field(w, field_name(s));
				case TTypeExpr(TClassDecl({ cl_path = (::(haxe, []), Int32) })): write(w, path_s_import(e.epos, (new Tuple(::("haxe", []),
							"Int32")), []));
				case TTypeExpr(TClassDecl({ cl_path = (::(haxe, []), Int64) })): write(w, path_s_import(e.epos, (new Tuple(::("haxe", []),
							"Int64")), []));
				case TTypeExpr(mt): write(w, md_s(e.epos, mt));
				case TParenthesis(e): write(w, "[");
					expr_s(w, e);
					write(w, "]");
				case TMeta(_, e): expr_s(w, e);
				case TCall({ eexpr = TLocal({ v_name = __array__ }) }, el) | TCall({ eexpr = TField(_, FStatic({ cl_path = (::(java, []), NativeArray) }, { cf_name = make })) }, el) | TArrayDecl(el) if (t_has_type_param(e.etype))
							: var Tuple(_, el) = extract_tparams([], el);
					print(w, "[ [%s] [new %s ", t_s(e.epos, e.etype), t_s(e.epos, replace_type_param(e.etype)));
					write(w, "{");
					ignore(List.fold_left(function acc: function e:
					if (<>(acc, 0)) {
					write(w, ", ");
					} else {
						[];
					};
					expr_s(w, e);
					+(acc, 1), 0, el));
					write(w, "}] ]");
				case TCall({ eexpr = TLocal({ v_name = __array__ }) }, el) | TCall({ eexpr = TField(_, FStatic({ cl_path = (::(java, []), NativeArray) }, { cf_name = make })) }, el) | TArrayDecl(el)
						: var Tuple(_, el) = extract_tparams([], el);
					print(w, "new %s", param_t_s(e.epos, transform_nativearray_t(e.etype)));
					var is_double = switch (follow(e.etype)) {
					case TInst(_, ::(t, [])): if ( && (like_float(t), !(like_int(t)))) {
							Some(t);
						} else {
							None;
						};
					case _: None;
					};
					write(w, "{");
					ignore(List.fold_left(function acc: function e:
					if (<>(acc, 0)) {
					write(w, ", ");
					} else {
						[];
					};
					var e = if (is_some(is_double)) {
					mk_cast(get(is_double), e);
					} else {
						e;
					};
					expr_s(w, e);
					+(acc, 1), 0, el));
					write(w, "}");
				case TCall({ eexpr = TField(_, FStatic({ cl_path = ([], String) }, { cf_name = fromCharCode })) }, ::(cc, [])): write(w,
							"Character.toString[[char] ");
					expr_s(w, cc);
					write(w, "]");
				case TCall({ eexpr = TLocal({ v_name = __is__ }) }, ::(expr, ::({ eexpr = TTypeExpr(md) }, []))): write(w, "[ ");
					expr_s(w, expr);
					write(w, " instanceof ");
					write(w, md_s(e.epos, md));
					write(w, " ]");
				case TCall({ eexpr = TLocal({ v_name = __java__ }) }, ::({ eexpr = TConst(TString(s)) }, [])): write(w, s);
				case TCall({ eexpr = TLocal({ v_name = __java__ }) }, ::({ eexpr = TConst(TString(s)) }, tl)): Codegen.interpolate_code(
						gen.gcon, s, tl, write(w), expr_s(w), e.epos);
				case TCall({ eexpr = TLocal({ v_name = __lock__ }) }, ::(eobj, ::(eblock, []))): write(w, "synchronized[");
					function loop(eobj) return {
						switch (eobj.eexpr) {
						case TTypeExpr(md): expr_s(w, eobj);
							write(w, ".class");
						case TMeta(_, e) | TParenthesis(e): loop(e);
						case _: expr_s(w, eobj);
						};
					};
					loop(eobj);
					write(w, "]");
					switch (eblock.eexpr) {
					case TBlock(::(_, _)): expr_s(w, eblock);
					case _: begin_block(w);
						expr_s(w, eblock);
						if (has_semicolon(eblock)) {
							write(w, ";");
						} else {
							[];
						};
						end_block(w);
					};
				case TCall({ eexpr = TLocal({ v_name = __goto__ }) }, ::({ eexpr = TConst(TInt(v)) }, [])): print(w, "break label%ld", v);
				case TCall({ eexpr = TLocal({ v_name = __label__ }) }, ::({ eexpr = TConst(TInt(v)) }, [])): print(w, "label%ld:", v);
				case TCall({ eexpr = TLocal({ v_name = __typeof__ }) }, ::({ eexpr = TTypeExpr(md) } = expr, [])): expr_s(w, expr);
					write(w, ".class");
				case TCall(e, el): var Tuple(params, el) = extract_tparams([], el);
					expr_s(w, e);
					write(w, "[");
					ignore(List.fold_left(function acc: function e:
					if (<>(acc, 0)) {
					write(w, ", ");
					} else {
						[];
					};
					expr_s(w, e);
					+(acc, 1), 0, el));
					write(w, "]");
				case TNew({ cl_path = (::(java, []), NativeArray) } = cl, params, ::(size, [])): 	function check_t_s(t, times) return {
						switch (real_type(t)) {
						case TInst({ cl_path = (::(java, []), NativeArray) }, ::(param, [])): check_t_s(param, +(times, 1));
						case _: print(w, "new %s[", t_s(e.epos, transform_nativearray_t(t)));
							expr_s(w, size);
							print(w, "]");
							function loop(i) return {
								if ( <= (i, 0)) {
									[];
								} else {
									write(w, "[]");
									loop(-(i, 1));
								};
							};
							loop(-(times, 1));
						};
					};
					check_t_s(TInst(cl, params), 0);
				case TNew({ cl_path = ([], String) } = cl, [], el): write(w, "new ");
					write(w, t_s(e.epos, TInst(cl, [])));
					write(w, "[");
					ignore(List.fold_left(function acc: function e:
					if (<>(acc, 0)) {
					write(w, ", ");
					} else {
						[];
					};
					expr_s(w, e);
					+(acc, 1), 0, el));
					write(w, "]");
				case TNew({ cl_kind = KTypeParameter(_) } = cl, params, el): print(w,
							"null /* This code should never be reached. It was produced by the use of @:generic on a new type parameter instance: %s */",
							path_param_s(e.epos, TClassDecl(cl), cl.cl_path, params, cl.cl_meta));
				case TNew(cl, params, el): write(w, "new ");
					write(w, path_param_s(e.epos, TClassDecl(cl), cl.cl_path, params, cl.cl_meta));
					write(w, "[");
					ignore(List.fold_left(function acc: function e:
					if (<>(acc, 0)) {
					write(w, ", ");
					} else {
						[];
					};
					expr_s(w, e);
					+(acc, 1), 0, el));
					write(w, "]");
				case TUnop(Ast.Increment = op, flag, e) | TUnop(Ast.Decrement = op, flag, e): switch (flag) {
					case Ast.Prefix: write(w, ^ (" ", ^ (Ast.s_unop(op), " ")));
						expr_s(w, e);
					case Ast.Postfix: expr_s(w, e);
						write(w, Ast.s_unop(op));
					};
				case TUnop(op, flag, e): switch (flag) {
					case Ast.Prefix: write(w, ^ (" ", ^ (Ast.s_unop(op), ", [")));
						expr_s(w, e);
						write(w, "] ");
					case Ast.Postfix: write(w, "[");
						expr_s(w, e);
						write(w, ^ ("] ", Ast.s_unop(op)));
					};
				case TVar(var, eopt): print(w, "%s ", t_s(e.epos, var.v_type));
					write_id(w, var.v_name);
					switch (eopt) {
					case None: write(w, " = ");
						expr_s(w, null(var.v_type, e.epos));
					case Some(e): write(w, " = ");
						expr_s(w, e);
					};
				case TBlock(::(e, [])) if (was_in_value): expr_s(w, e);
				case TBlock(el): begin_block(w);
					List.iter(function e: List.iter(function e: in_value.val = False;
													line_directive(w, e.epos);
													expr_s(w, e);
					if (has_semicolon(e)) {
					write(w, ";");
					} else {
						[];
					};
					newline(w), extract_statements(e)), el);
					end_block(w);
				case TIf(econd, e1, Some(eelse)) if (was_in_value): write(w, "[ ");
					expr_s(w, mk_paren(econd));
					write(w, " ? ");
					expr_s(w, mk_paren(e1));
					write(w, " : ");
					expr_s(w, mk_paren(eelse));
					write(w, " ]");
				case TIf(econd, e1, eelse): write(w, "if ");
					expr_s(w, mk_paren(econd));
					write(w, " ");
					in_value.val = False;
					expr_s(w, mk_block(e1));
					switch (eelse) {
					case None: [];
					case Some(e): write(w, "else");
						in_value.val = False;
						expr_s(w, mk_block(e));
					};
				case TWhile(econd, eblock, flag): switch (flag) {
					case Ast.NormalWhile: write(w, "while ");
						expr_s(w, mk_paren(econd));
						write(w, "");
						in_value.val = False;
						expr_s(w, mk_block(eblock));
					case Ast.DoWhile: write(w, "do ");
						in_value.val = False;
						expr_s(w, mk_block(eblock));
						write(w, "while ");
						in_value.val = True;
						expr_s(w, mk_paren(econd));
					};
				case TSwitch(econd, ele_l, default): write(w, "switch ");
					expr_s(w, mk_paren(econd));
					begin_block(w);
					List.iter(function (el, e): List.iter(function e: write(w, "case ");
														  in_value.val = True;
					switch (e.eexpr) {
				case TField(_, FEnum(e, ef)): var changed_name = change_id(ef.ef_name);
						write(w, changed_name);
					case _: expr_s(w, e);
					};
					write(w, ":");
					newline(w), el);
					in_value.val = False;
								   expr_s(w, mk_block(e));
								   newline(w);
								   newline(w), ele_l);
					if (is_some(default)) {
						write(w, "default:");
						newline(w);
						in_value.val = False;
						expr_s(w, get(default));
						newline(w);
					} else {
						[];
					};
					end_block(w);
				case TTry(tryexpr, ve_l): write(w, "try ");
					in_value.val = False;
					expr_s(w, mk_block(tryexpr));
					var pos = e.epos;
					List.iter(function (var, e): print(w, "catch [%s %s]", t_s(pos, var.v_type), var.v_name);
							  in_value.val = False;
							  expr_s(w, mk_block(e));
							  newline(w), ve_l);
				case TReturn(eopt): write(w, "return ");
					if (is_some(eopt)) {
						expr_s(w, get(eopt));
					} else {
						[];
					};
				case TBreak: write(w, "break");
				case TContinue: write(w, "continue");
				case TThrow(e): write(w, "throw ");
					expr_s(w, e);
				case TCast(e1, md_t): print(w, "[[%s] [", t_s(e.epos, e.etype));
					expr_s(w, e1);
					write(w, "] ]");
				case TFor(_, _, content): write(w, "[ for not supported ");
					expr_s(w, content);
					write(w, " ]");
					if (strict_mode.val) {
						assert False;
					} else {
						[];
					};
				case TObjectDecl(_): write(w, "[ obj decl not supported ]");
					if (strict_mode.val) {
						assert False;
					} else {
						[];
					};
				case TFunction(_): write(w, "[ func decl not supported ]");
					if (strict_mode.val) {
						assert False;
					} else {
						[];
					};
				case TEnumParameter(_): write(w, "[ enum parameter not supported ]");
					if (strict_mode.val) {
						assert False;
					} else {
						[];
					};
				};
			};
			expr_s(w, e);
		};
		function gen_fpart_attrib(w) return {
		case (EConst(Ident(i)), _): write(w, i);
		case (EField(ef, f), _): gen_fpart_attrib(w, ef);
			write(w, ".");
			write(w, f);
		case (_, p): gen.gcon.error("Invalid expression inside @:meta metadata", p);
		};
		function gen_spart(w) return {
		case (EConst(c), p): switch (c) {
			case Int(s) | Float(s) | Ident(s): write(w, s);
			case String(s): write(w, "\"");
				write(w, escape(s));
				write(w, "\"");
			case _: gen.gcon.error("Invalid expression inside @:meta metadata", p);
			};
		case (EField(ef, f), _): gen_spart(w, ef);
			write(w, ".");
			write(w, f);
		case (EBinop(Ast.OpAssign, (EConst(Ident(s)), _), e2), _): write(w, s);
			write(w, " = ");
			gen_spart(w, e2);
		case (EArrayDecl(el), _): write(w, "{");
			var fst = ref(True);
			List.iter(function e:
			if (fst.val) {
			fst.val = False;
		} else {
			write(w, ", ");
			};
			gen_spart(w, e), el);
			write(w, "}");
		case (ECall(fpart, args), _): gen_fpart_attrib(w, fpart);
			write(w, "[");
			var fst = ref(True);
			List.iter(function e:
			if (fst.val) {
			fst.val = False;
		} else {
			write(w, ", ");
			};
			gen_spart(w, e), args);
			write(w, "]");
		case (_, p): gen.gcon.error("Invalid expression inside @:meta metadata", p);
		};
		function gen_annotations(w, ? : (add_newline = True), metadata) return {
		List.iter(function case (Meta.Meta, ::(meta, []), _): write(w, "@");
				gen_spart(w, meta);
				if (add_newline) {
				newline(w);
				} else {
					write(w, " ");
				};
		case _: [], metadata);
		};
		function argt_s(p, t) return {
			var w = new_source_writer([]);
			function run(t) return {
				switch (t) {
				case TType(tdef, p): gen_annotations(w, add_newline = False, tdef.t_meta);
					run(follow_once(t));
				case TMono(r): switch (r.val) {
					case Some(t): run(t);
					case _: [];
					};
				case TLazy(f): run(f.val([]));
				case _: [];
				};
			};
			run(t);
			var ret = t_s(p, t);
			var c = contents(w);
			if (<>(c, "")) {
				^ (c, ^ (" ", ret));
			} else {
				ret;
			};
		};
		function get_string_params(cl_params) return {
			switch (cl_params) {
			case []: (new Tuple("", ""));
			case _: var params = sprintf("<%s>", String.concat(", ", List.map(function (_, tcl): switch (follow(tcl)) {
			case TInst(cl, _): snd(cl.cl_path);
				case _: assert False;
				}, cl_params)));
				var params_extends = List.fold_left(function acc: function (name, t):
				switch (run_follow(gen, t)) {
			case TInst(cl, p): switch (cl.cl_implements) {
					case []: acc;
					case _: acc;
					};
				case _: trace(t_s(Ast.null_pos, t));
					assert False;
				}, [], cl_params);
				(new Tuple(params, String.concat(" ", params_extends)));
			};
		};
		function write_parts(w, parts) return {
			var parts = List.filter(function s: <>(s, ""), parts);
			write(w, String.concat(" ", parts));
		};
		function gen_class_field(w, ? : (is_overload = False), is_static, cl, is_final, cf) return {
			var is_interface = cl.cl_interface;
			var Tuple(name, is_new, is_explicit_iface) = switch (cf.cf_name) {
			case new: (new Tuple(snd(cl.cl_path), True, False));
			case name if (String.contains(name, '.')): var Tuple(fn_name, path) = parse_explicit_iface(name);
				(new Tuple( ^ (path_s(path, cl.cl_meta), ^ (".", fn_name)), False, True));
			case name: (new Tuple(name, False, False));
			};
			switch (cf.cf_kind) {
			case Var(_) | Method(MethDynamic) if (!(Type.is_extern_field(cf))):
				if ( || (is_overload, List.exists(function cf: <>(cf.cf_expr, None), cf.cf_overloads))) {
					gen.gcon.error("Only normal [non-dynamic] methods can be overloaded", cf.cf_pos);
				} else {
					[];
				};
				if (!(is_interface)) {
					var Tuple(access, modifiers) = get_fun_modifiers(cf.cf_meta, "public", []);
					write_parts(w, @(::(access, ::(if (is_static) {
					"static";
				} else {
					"";
				}, modifiers)), ::(t_s(cf.cf_pos, run_follow(gen, cf.cf_type)), ::(change_field(name), []))));
					switch (cf.cf_expr) {
					case Some(e): write(w, " = ");
						expr_s(w, e);
						write(w, ";");
					case None: write(w, ";");
					};
				} else {
					[];
				};
			case Method(_) if (||(Type.is_extern_field(cf), switch ((new Tuple(cl.cl_kind, cf.cf_expr))) {
					case (KAbstractImpl(_), None): True;
						case _: False;
						})): List.iter(function cf:
				if ( || (cl.cl_interface, <>(cf.cf_expr, None))) {
				gen_class_field(w, is_overload = True, is_static, cl, Meta.has(Meta.Final, cf.cf_meta), cf);
				} else {
					[];
				}, cf.cf_overloads);
			case Var(_) | Method(MethDynamic): [];
			case Method(mkind): List.iter(function cf: if ( || (cl.cl_interface, <>(cf.cf_expr, None))) {
				gen_class_field(w, is_overload = True, is_static, cl, Meta.has(Meta.Final, cf.cf_meta), cf);
				} else {
					[];
				}, cf.cf_overloads);
				var is_virtual = || (is_new, && (!(is_final), switch (mkind) {
			case MethInline: False;
			case _ if (!(is_new)): True;
				case _: False;
				}));
				var is_override = switch (cf.cf_name) {
				case equals if (!(is_static)):
					switch (cf.cf_type) {
					case TFun(::((_, _, t), []), ret): switch ((new Tuple(real_type(t), real_type(ret)))) {
						case (TDynamic(_), TAbstract({ a_path = ([], Bool) }, [])) | (TAnon(_), TAbstract({ a_path = ([], Bool) }, [])): True;
						case _: List.memq(cf, cl.cl_overrides);
						};
					case _: List.memq(cf, cl.cl_overrides);
					};
				case toString if (!(is_static)):
					switch (cf.cf_type) {
					case TFun([], ret): switch (real_type(ret)) {
						case TInst({ cl_path = ([], String) }, []): True;
						case _: gen.gcon.error("A toString[] function should return a String!", cf.cf_pos);
							False;
						};
					case _: List.memq(cf, cl.cl_overrides);
					};
				case hashCode if (!(is_static)):
					switch (cf.cf_type) {
					case TFun([], ret): switch (real_type(ret)) {
						case TAbstract({ a_path = ([], Int) }, []): True;
						case _: gen.gcon.error("A hashCode[] function should return an Int!", cf.cf_pos);
							False;
						};
					case _: List.memq(cf, cl.cl_overrides);
					};
				case _: List.memq(cf, cl.cl_overrides);
				};
				var visibility = if (is_interface) {
					"";
				} else {
					"public";
				};
				var Tuple(visibility, modifiers) = get_fun_modifiers(cf.cf_meta, visibility, []);
				var Tuple(visibility, is_virtual) = if (is_explicit_iface) {
					(new Tuple("", False));
				} else {
					(new Tuple(visibility, is_virtual));
				};
				var v_n = if (is_static) {
					"static";
				} else {
					if ( && (is_override, !(is_interface))) {
						"";
					} else {
						if (!(is_virtual)) {
							"final";
						} else {
							"";
						};
					};
				};
				var cf_type = if ( && (is_override, && (!(is_overload), !(Meta.has(Meta.Overload, cf.cf_meta))))) {
					switch (field_access(gen, TInst(cl, List.map(snd, cl.cl_params)), cf.cf_name)) {
					case FClassField(_, _, _, _, _, actual_t, _): actual_t;
					case _: assert False;
					};
				} else {
					cf.cf_type;
				};
				var params = List.map(snd, cl.cl_params);
				var Tuple(ret_type, args) = switch ((new Tuple(follow(cf_type), follow(cf.cf_type)))) {
				case (TFun(strbtl, t), TFun(rargs, _)): (new Tuple(apply_params(cl.cl_params, params, real_type(t)), List.map2(function (_,
							_, t): function (n, o, _): (new Tuple(n, o, apply_params(cl.cl_params, params, real_type(t)))), strbtl, rargs)));
				case _: assert False;
				};
				if ( && (is_override, !(is_interface))) {
					write(w, "@Override ");
				} else {
					[];
				};
				gen_annotations(w, cf.cf_meta);
				var Tuple(params, _) = get_string_params(cf.cf_params);
				write_parts(w, @(::(visibility, ::(v_n, modifiers)), ::(params, ::(if (is_new) {
				"";
			} else {
				rett_s(cf.cf_pos, run_follow(gen, ret_type));
				}, ::(change_field(name), [])))));
				switch (cf.cf_expr) {
				case Some({ eexpr = TFunction(tf) }): print(w, "[%s]", String.concat(", ", List.map2(function (var, _): function (_, _,
							t): sprintf("%s %s", argt_s(cf.cf_pos, run_follow(gen, t)), change_id(var.v_name)), tf.tf_args, args)));
				case _: print(w, "[%s]", String.concat(", ", List.map(function (name, _, t): sprintf("%s %s", argt_s(cf.cf_pos,
														   run_follow(gen, t)), change_id(name)), args)));
				};
				if ( || (is_interface, List.mem("native", modifiers))) {
					write(w, ";");
				} else {
					function loop(meta) return {
						switch (meta) {
						case []: var expr = switch (cf.cf_expr) {
							case None: mk(TBlock([]), t_dynamic, Ast.null_pos);
							case Some(s): switch (s.eexpr) {
								case TFunction(tf): mk_block(tf.tf_expr);
								case _: assert False;
								};
							};
							if (is_new) {
								expr_s(w, expr);
							} else {
								expr_s(w, expr);
							};
						case ::((Meta.Throws, ::((Ast.EConst(Ast.String(t)), _), []), _), tl): print(w, " throws %s", t);
							loop(tl);
						case ::((Meta.FunctionCode, ::((Ast.EConst(Ast.String(contents)), _), []), _), tl): begin_block(w);
							write(w, contents);
							end_block(w);
						case ::(_, tl): loop(tl);
						};
					};
					loop(cf.cf_meta);
				};
			};
			newline(w);
			newline(w);
		};
		function gen_class(w, cl) return {
			var cf_filters = ::(handle_throws, []);
			List.iter(function f: List.iter(f(gen), cl.cl_ordered_fields), cf_filters);
			List.iter(function f: List.iter(f(gen), cl.cl_ordered_statics), cf_filters);
			var should_close = switch (change_ns(fst(cl.cl_path))) {
			case []: False;
			case ns: print(w, "package %s;", String.concat(".", change_ns(ns)));
				newline(w);
				newline(w);
				False;
			};
			function loop_meta(meta, acc) return {
				switch (meta) {
				case ::((Meta.SuppressWarnings, ::((Ast.EConst(Ast.String(w)), _), []), _), meta): loop_meta(meta, ::(w, acc));
				case ::(_, meta): loop_meta(meta, acc);
				case _: acc;
				};
			};
			var suppress_warnings = loop_meta(cl.cl_meta, ::("rawtypes", ::("unchecked", [])));
			write(w, "import haxe.root.*;");
			newline(w);
			var w_header = w;
			var w = new_source_writer([]);
			clear_scope([]);
		List.iter(function case TClassDecl({ cl_path = ([], c) }): imports.val = ::((new Tuple([], c)), imports.val);
					  case TEnumDecl({ e_path = ([], c) }): imports.val = ::((new Tuple([], c)), imports.val);
						  case TAbstractDecl({ a_path = ([], c) }): imports.val = ::((new Tuple([], c)), imports.val);
							  case _: [], gen.gtypes_list);
			newline(w);
			write(w, "@SuppressWarnings[value={");
			var first = ref(True);
			List.iter(function s:
			if (first.val) {
			first.val = False;
		} else {
			write(w, ", ");
			};
			print(w, "\"%s\"", escape(s)), suppress_warnings);
			write(w, "}]");
			newline(w);
			gen_annotations(w, cl.cl_meta);
			var Tuple(clt, access, modifiers) = get_class_modifiers(cl.cl_meta, if (cl.cl_interface) {
			"interface";
		} else {
			"class";
		}, "public", []);
			var is_final = Meta.has(Meta.Final, cl.cl_meta);
			write_parts(w, @(::(access, modifiers), ::(clt, ::(change_clname(snd(cl.cl_path)), []))));
			var Tuple(params, _) = get_string_params(cl.cl_params);
			function cl_p_to_string(Tuple(c, p)) return {
				var p = List.map(function t:
				switch (follow(t)) {
			case TMono(_) | TDynamic(_): t_empty;
				case _: t;
				}, p);
				path_param_s(cl.cl_pos, TClassDecl(c), c.cl_path, p, c.cl_meta);
			};
			print(w, "%s", params);
			if (is_some(cl.cl_super)) {
				print(w, " extends %s", cl_p_to_string(get(cl.cl_super)));
			} else {
				[];
			};
			switch (cl.cl_implements) {
			case []: [];
			case _: print(w, " %s %s", if (cl.cl_interface) {
				"extends";
			} else {
				"implements";
			}, String.concat(", ", List.map(cl_p_to_string, cl.cl_implements)));
			};
			begin_block(w);
			function loop(cl) return {
				List.iter(function cf: add_scope(cf.cf_name), cl.cl_ordered_fields);
				List.iter(function cf: add_scope(cf.cf_name), cl.cl_ordered_statics);
				switch (cl.cl_super) {
				case Some(c, _): loop(c);
				case None: [];
				};
			};
			loop(cl);
			function loop(meta) return {
				switch (meta) {
				case []: [];
				case ::((Meta.ClassCode, ::((Ast.EConst(Ast.String(contents)), _), []), _), tl): write(w, contents);
				case ::(_, tl): loop(tl);
				};
			};
			loop(cl.cl_meta);
			switch (gen.gcon.main_class) {
			case Some(path) if (=(path, cl.cl_path)): write(w, "public static void main[String[] args]");
				begin_block(w);
				try {
					var t = Hashtbl.find(gen.gtypes, (new Tuple([], "Sys")));
					switch (t) {
					case TClassDecl(cl) if (PMap.mem("_args", cl.cl_statics)): write(w, "Sys._args = args;");
						newline(w);
					case _: [];
					};
				} catch (e: Not_found) {
					[];
				};
				write(w, "main[];");
				end_block(w);
				newline(w);
			case _: [];
			};
			switch (cl.cl_init) {
			case None: [];
			case Some(init): write(w, "static");
				expr_s(w, mk_block(init));
				newline(w);
			};
			if (is_some(cl.cl_constructor)) {
				gen_class_field(w, False, cl, is_final, get(cl.cl_constructor));
			} else {
				[];
			};
			if (!(cl.cl_interface)) {
				List.iter(gen_class_field(w, True, cl, is_final), cl.cl_ordered_statics);
			} else {
				[];
			};
			List.iter(gen_class_field(w, False, cl, is_final), cl.cl_ordered_fields);
			end_block(w);
			if (should_close) {
				end_block(w);
			} else {
				[];
			};
		List.iter(function case (::(haxe, ::(root, [])), _) | ([], _): [];
					  case path: write(w_header, "import ");
							  write(w_header, path_s(path, []));
							  write(w_header, ";\n"), imports.val);
			add_writer(w, w_header);
		};
		function gen_enum(w, e) return {
			var should_close = switch (change_ns(fst(e.e_path))) {
			case []: False;
			case ns: print(w, "package %s;", String.concat(".", change_ns(ns)));
				newline(w);
				newline(w);
				False;
			};
			gen_annotations(w, e.e_meta);
			print(w, "public enum %s", change_clname(snd(e.e_path)));
			begin_block(w);
			write(w, String.concat(", ", List.map(change_id, e.e_names)));
			end_block(w);
			if (should_close) {
				end_block(w);
			} else {
				[];
			};
		};
		function module_type_gen(w, md_tp) return {
			Codegen.map_source_header(gen.gcon, function s: print(w, "// %s\n", s));
			switch (md_tp) {
			case TClassDecl(cl): if (!(cl.cl_extern)) {
					gen_class(w, cl);
					newline(w);
					newline(w);
				} else {
					[];
				};
				!(cl.cl_extern);
			case TEnumDecl(e): if ( && (!(e.e_extern), !(Meta.has(Meta.Class, e.e_meta)))) {
					gen_enum(w, e);
					newline(w);
					newline(w);
				} else {
					[];
				};
				!(e.e_extern);
			case TTypeDecl(e): False;
			case TAbstractDecl(a): False;
			};
		};
		function module_gen(w, md) return {
			module_type_gen(w, md);
		};
		init_ctx(gen);
		Hashtbl.add(gen.gspecial_vars, "__label__", True);
		Hashtbl.add(gen.gspecial_vars, "__goto__", True);
		Hashtbl.add(gen.gspecial_vars, "__is__", True);
		Hashtbl.add(gen.gspecial_vars, "__typeof__", True);
		Hashtbl.add(gen.gspecial_vars, "__java__", True);
		Hashtbl.add(gen.gspecial_vars, "__lock__", True);
		Hashtbl.add(gen.gspecial_vars, "__array__", True);
		gen.greal_type = real_type;
		gen.greal_type_param = change_param_type;
		SetHXGen.run_filter(gen, SetHXGen.default_hxgen_func);
		var run_follow_gen = run_follow(gen);
		function type_map(e) return {
			Type.map_expr_type(function e: type_map(e), run_follow_gen, function tvar: tvar.v_type = run_follow_gen(tvar.v_type);
			tvar, e);
		};
		function super_map(Tuple(cl, tl)) return {
			(new Tuple(cl, List.map(run_follow_gen, tl)));
		};
	List.iter(function case TClassDecl(cl): var all_fields = @(Option.map_default(function cf: ::(cf, []), [], cl.cl_constructor), @(cl.cl_ordered_fields, cl.cl_ordered_statics));
					  List.iter(function cf: cf.cf_type = run_follow_gen(cf.cf_type);
								cf.cf_expr = Option.map(type_map, cf.cf_expr), all_fields);
					  cl.cl_dynamic = Option.map(run_follow_gen, cl.cl_dynamic);
					  cl.cl_array_access = Option.map(run_follow_gen, cl.cl_array_access);
					  cl.cl_init = Option.map(type_map, cl.cl_init);
					  cl.cl_super = Option.map(super_map, cl.cl_super);
					  cl.cl_implements = List.map(super_map, cl.cl_implements);
				  case _: [], gen.gtypes_list);
		var closure_t = ClosuresToClass.DoubleAndDynamicClosureImpl.get_ctx(gen, 6);
		function get_vmtype(t) return {
			switch (real_type(t)) {
			case TInst({ cl_path = (::(java, []), NativeArray) }, tl): t;
			case TInst(c, tl): TInst(c, List.map(function _: t_dynamic, tl));
			case TEnum(e, tl): TEnum(e, List.map(function _: t_dynamic, tl));
			case TType(t, tl): TType(t, List.map(function _: t_dynamic, tl));
			case TAbstract(a, tl): TAbstract(a, List.map(function _: t_dynamic, tl));
			case t: t;
			};
		};
		FixOverrides.configure(get_vmtype = , gen);
		Normalize.configure(gen, metas = Hashtbl.create(0));
		AbstractImplementationFix.configure(gen);
		IteratorsInterface.configure(gen, function e: e);
		ClosuresToClass.configure(gen, ClosuresToClass.default_implementation(closure_t, get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "Function"))))));
		var enum_base = get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "Enum"))));
		var param_enum_base = get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "ParamEnum"))));
		EnumToClass.configure(gen, None, False, True, enum_base, param_enum_base, False, False);
		InterfaceVarsDeleteModf.configure(gen);
		var dynamic_object = get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "DynamicObject"))));
		var object_iface = get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "IHxObject"))));
		var empty_e = switch (get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "EmptyObject")))) {
		case TEnumDecl(e): e;
		case _: assert False;
		};
		var empty_expr = { () with eexpr = TTypeExpr(TEnumDecl(empty_e));
						   etype = TAnon({ () with a_fields = PMap.empty;
										   a_status = ref(EnumStatics(empty_e))
										 });
						   epos = null_pos
						 };
		var empty_ef = try {
			PMap.find("EMPTY", empty_e.e_constrs);
		} catch (e: Not_found) {
			gen.gcon.error("Required enum field EMPTY was not found", empty_e.e_pos);
			assert False;
		};
		OverloadingConstructor.configure(empty_ctor_type = TEnum(empty_e, []), empty_ctor_expr = { () with eexpr = TField(empty_expr, FEnum(empty_e, empty_ef));
																								   etype = TEnum(empty_e, []);
																								   epos = null_pos
																								 }, supports_ctor_inheritance = False, gen);
		var rcf_static_find = mk_static_field_access_infer(get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "FieldLookup")))), "findHash", Ast.null_pos, []);
		function get_specialized_postfix(t) return {
			switch (t) {
			case TAbstract({ a_path = ([], Float) }, _): "Float";
			case TInst({ cl_path = ([], String) }, _): "String";
			case TAnon(_) | TDynamic(_): "Dynamic";
			case _: print_endline(debug_type(t));
				assert False;
			};
		};
		function rcf_static_insert(t) return {
			mk_static_field_access_infer(get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "FieldLookup")))), ^ ("insert", get_specialized_postfix(t)), Ast.null_pos, []);
		};
		function rcf_static_remove(t) return {
			mk_static_field_access_infer(get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "FieldLookup")))), ^ ("remove", get_specialized_postfix(t)), Ast.null_pos, []);
		};
		function can_be_float(t) return {
			like_float(real_type(t));
		};
		function rcf_on_getset_field(main_expr, field_expr, field, may_hash, may_set, is_unsafe) return {
			var is_float = can_be_float(if (is_none(may_set)) {
			main_expr.etype;
		} else {
			get(may_set).etype;
			});
			var fn_name = if (is_some(may_set)) {
				"setField";
			} else {
				"getField";
			};
			var fn_name = if (is_float) {
				^ (fn_name, "_f");
			} else {
				fn_name;
			};
			var pos = field_expr.epos;
			var is_unsafe = { () with eexpr = TConst(TBool(is_unsafe));
							  etype = basic.tbool;
							  epos = pos
							};
			var should_cast = switch (main_expr.etype) {
			case TAbstract({ a_path = ([], Float) }, []): False;
			case _: True;
			};
			var infer = mk_static_field_access_infer(runtime_cl, fn_name, field_expr.epos, []);
			var first_args = @(::(field_expr, ::({ () with eexpr = TConst(TString(field));
												   etype = basic.tstring;
												   epos = pos
			}, [])), if (is_some(may_hash)) {
			::({ () with eexpr = TConst(TInt(get(may_hash)));
				 etype = basic.tint;
				 epos = pos
			   }, []);
			} else {
				[];
			});
			var args = @(first_args, switch ((new Tuple(is_float, may_set))) {
		case (True, Some(set)): ::(if (should_cast) {
				mk_cast(basic.tfloat, set);
				} else {
					set;
				}, []);
			case (False, Some(set)): ::(set, []);
			case _: ::(is_unsafe, []);
			});
			var call = { (main_expr) with eexpr = TCall(infer, args) };
			var call = if ( && (is_float, should_cast)) {
				mk_cast(main_expr.etype, call);
			} else {
				call;
			};
			call;
		};
		function rcf_on_call_field(ecall, field_expr, field, may_hash, args) return {
			var infer = mk_static_field_access_infer(runtime_cl, "callField", field_expr.epos, []);
			var hash_arg = switch (may_hash) {
			case None: [];
			case Some(h): ::({
					() with eexpr = TConst(TInt(h));
					etype = basic.tint;
					epos = field_expr.epos
				}, []);
			};
			var arr_call = if (<>(args, [])) {
				{
					() with eexpr = TArrayDecl(args);
					etype = basic.tarray(t_dynamic);
					epos = ecall.epos
				};
			} else {
				null(basic.tarray(t_dynamic), ecall.epos);
			};
			var call_args = @(::(field_expr, ::({ (field_expr) with eexpr = TConst(TString(field));
												  etype = basic.tstring
												}, [])), @(hash_arg, ::(arr_call, [])));
			mk_cast(ecall.etype, { (ecall) with eexpr = TCall(infer, call_args);
								   etype = t_dynamic
								 });
		};
		var rcf_ctx = ReflectionCFs.new_ctx(gen, closure_t, object_iface, False, rcf_on_getset_field, rcf_on_call_field, function hash: function hash_array: function length: { (hash) with eexpr = TCall(rcf_static_find, ::(hash, ::(hash_array, ::(length, []))));
											etype = basic.tint
		}, function hash: hash, function hash_array: function length: function pos: function value: { (hash_array) with eexpr = TBinop(OpAssign, hash_array, mk(TCall(rcf_static_insert(value.etype), ::(hash_array, ::(length, ::(pos, ::(value, []))))), hash_array.etype, hash_array.epos)) }, function hash_array: function length: function pos: var t = gen.gclasses.nativearray_type(hash_array.etype);
		{
			(hash_array) with eexpr = TCall(rcf_static_remove(t), ::(hash_array, ::(length, ::(pos, []))));
			etype = gen.gcon.basic.tvoid
		}, False);
		ReflectionCFs.UniversalBaseClass.default_config(gen, get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "HxObject")))), object_iface, dynamic_object);
		ReflectionCFs.configure_dynamic_field_access(rcf_ctx, False);
		var closure_cl = get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "Closure"))));
		var closure_func = ReflectionCFs.get_closure_func(rcf_ctx, closure_cl);
		ReflectionCFs.implement_varargs_cl(rcf_ctx, get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "VarArgsBase")))));
		var slow_invoke = mk_static_field_access_infer(runtime_cl, "slowCallField", Ast.null_pos, []);
		ReflectionCFs.configure(rcf_ctx, slow_invoke = function ethis: function efield: function eargs: {
			() with eexpr = TCall(slow_invoke, ::(ethis, ::(efield, ::(eargs, []))));
			etype = t_dynamic;
			epos = ethis.epos
		}, object_iface);
		var objdecl_fn = ReflectionCFs.implement_dynamic_object_ctor(rcf_ctx, dynamic_object);
		ObjectDeclMap.configure(gen, ObjectDeclMap.traverse(gen, objdecl_fn));
		InitFunction.configure(gen, True, True);
		TArrayTransform.configure(gen, TArrayTransform.default_implementation(gen, function e: function _:
		switch (e.eexpr) {
	case TArray({ eexpr = TLocal({ v_extra = Some(::(_, _), _) }) }, _): False;
		case TArray(e1, e2): switch (run_follow(gen, follow(e1.etype))) {
			case TInst({ cl_path = (::(java, []), NativeArray) }, _): False;
			case _: True;
			};
		case _: assert False;
		}, "__get", "__set"));
		function field_is_dynamic(t, field) return {
			switch (field_access_esp(gen, gen.greal_type(t), field)) {
			case FClassField(cl, p, _, _, _, t, _): var p = change_param_type(TClassDecl(cl), p);
				is_dynamic(apply_params(cl.cl_params, p, t));
			case FEnumField(_): False;
			case _: True;
			};
		};
		function is_type_param(e) return {
			switch (follow(e)) {
			case TInst({ cl_kind = KTypeParameter(_) }, []): True;
			case _: False;
			};
		};
		function is_dynamic_expr(e) return {
			|| (is_dynamic(e.etype), switch (e.eexpr) {
		case TField(tf, f): field_is_dynamic(tf.etype, f);
			case _: False;
			});
		};
		function may_nullable(t) return {
			switch (gen.gfollowrun_f(t)) {
			case TType({ t_path = ([], Null) }, ::(t, [])):
				switch (follow(t)) {
				case TInst({ cl_path = ([], String) }, []) | TAbstract({ a_path = ([], Float) }, []) | TInst({ cl_path = (::(haxe, []), Int32) }, []) | TInst({ cl_path = (::(haxe, []), Int64) }, []) | TAbstract({ a_path = ([], Int) }, []) | TAbstract({ a_path = ([], Bool) }, [])
						: Some(t);
				case t if (is_java_basic_type(t)): Some(t);
				case _: None;
				};
			case _: None;
			};
		};
		function is_double(t) return {
			&& (like_float(t), !(like_int(t)));
		};
		function is_int(t) return {
			like_int(t);
		};
		DynamicOperators.configure(gen, DynamicOperators.abstract_implementation(gen, function e:
		switch (e.eexpr) {
	case TBinop(Ast.OpEq, e1, e2): || (is_dynamic(e1.etype), || (is_dynamic(e2.etype),
											   || (is_type_param(e1.etype), is_type_param(e2.etype))));
		case TBinop(Ast.OpAdd, e1, e2) | TBinop(Ast.OpNotEq, e1, e2): || (is_dynamic(e1.etype), || (is_dynamic(e2.etype),
					|| (is_type_param(e1.etype), is_type_param(e2.etype))));
		case TBinop(Ast.OpLt, e1, e2) | TBinop(Ast.OpLte, e1, e2) | TBinop(Ast.OpGte, e1, e2) | TBinop(Ast.OpGt, e1, e2):
			|| (is_dynamic(e.etype), || (is_dynamic_expr(e1), || (is_dynamic_expr(e2),
										 || (is_string(e1.etype), is_string(e2.etype)))));
		case TBinop(_, e1, e2): || (is_dynamic(e.etype), || (is_dynamic_expr(e1), is_dynamic_expr(e2)));
		case TUnop(_, _, e1): is_dynamic_expr(e1);
		case _: False;
		}, function e1: function e2: 	function is_null(e) return {
			switch (e.eexpr) {
			case TConst(TNull) | TLocal({ v_name = __undefined__ }): True;
			case _: False;
			};
		};
		switch ((new Tuple(e1.eexpr, e2.eexpr))) {
	case (TConst(c1), TConst(c2)) if (||(is_null(e1), is_null(e2))): {
			(e1) with eexpr = TConst(TBool( = (c1, c2)));
			etype = basic.tbool
		};
		case _ if (||(is_null(e1), &&(is_null(e2), !(||(is_java_basic_type(e1.etype), is_java_basic_type(e2.etype)))))): {
			(e1) with eexpr = TBinop(Ast.OpEq, e1, e2);
			etype = basic.tbool
		};
		case _: var is_ref = switch ((new Tuple(follow(e1.etype), follow(e2.etype)))) {
			case (TDynamic(_), _) | (_, TDynamic(_)) | (TAbstract({ a_path = ([], Float) }, []), _) | (TInst({ cl_path = (::(haxe, []), Int32) }, []), _) | (TInst({ cl_path = (::(haxe, []), Int64) }, []), _) | (TAbstract({ a_path = ([], Int) }, []), _) | (TAbstract({ a_path = ([], Bool) }, []), _) | (_, TAbstract({ a_path = ([], Float) }, [])) | (_, TAbstract({ a_path = ([], Int) }, [])) | (_, TInst({ cl_path = (::(haxe, []), Int32) }, [])) | (_, TInst({ cl_path = (::(haxe, []), Int64) }, [])) | (_, TAbstract({ a_path = ([], Bool) }, [])) | (TInst({ cl_kind = KTypeParameter(_) }, []), _) | (_, TInst({ cl_kind = KTypeParameter(_) }, []))
					: False;
			case (_, _): True;
			};
			var static = mk_static_field_access_infer(runtime_cl, if (is_ref) {
			"refEq";
		} else {
			"eq";
		}, e1.epos, []);
			{
				() with eexpr = TCall(static, ::(e1, ::(e2, [])));
				etype = gen.gcon.basic.tbool;
				epos = e1.epos
			};
		}, function e: function e1: function e2:
		switch ((new Tuple(may_nullable(e1.etype), may_nullable(e2.etype)))) {
	case (Some(t1), Some(t2)): var Tuple(t1, t2) = if ( || (is_string(t1), is_string(t2))) {
				(new Tuple(basic.tstring, basic.tstring));
			} else {
				if ( || (is_double(t1), is_double(t2))) {
					(new Tuple(basic.tfloat, basic.tfloat));
				} else {
					if ( || (is_int(t1), is_int(t2))) {
						(new Tuple(basic.tint, basic.tint));
					} else {
						(new Tuple(t1, t2));
					};
				};
			};
			{
				() with eexpr = TBinop(Ast.OpAdd, mk_cast(t1, e1), mk_cast(t2, e2));
				etype = e.etype;
				epos = e1.epos
			};
		case _: var static = mk_static_field_access_infer(runtime_cl, "plus", e1.epos, []);
			mk_cast(e.etype, { () with eexpr = TCall(static, ::(e1, ::(e2, [])));
							   etype = t_dynamic;
							   epos = e1.epos
							 });
		}, function e1: function e2:
		if (is_string(e1.etype)) {
		{
			(e1) with eexpr = TCall(mk_field_access(gen, e1, "compareTo", e1.epos), ::(e2, []));
				etype = gen.gcon.basic.tint
			};
		} else {
			var static = mk_static_field_access_infer(runtime_cl, "compare", e1.epos, []);
			{
				() with eexpr = TCall(static, ::(e1, ::(e2, [])));
				etype = gen.gcon.basic.tint;
				epos = e1.epos
			};
		}));
		FilterClosures.configure(gen, FilterClosures.traverse(gen, function e1: function s: True, closure_func));
		var base_exception = get_cl(get_type(gen, (new Tuple(::("java", ::("lang", [])), "Throwable"))));
		var base_exception_t = TInst(base_exception, []);
		var hx_exception = get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "HaxeException"))));
		var hx_exception_t = TInst(hx_exception, []);
		function is_exception(t) return {
			switch (follow(t)) {
			case TInst(cl, _): if ( == (cl, base_exception)) {
					True;
				} else {
					switch (cl.cl_super) {
					case None: False;
					case Some(cl, arg): is_exception(TInst(cl, arg));
					};
				};
			case _: False;
			};
		};
		TryCatchWrapper.configure(gen, TryCatchWrapper.traverse(gen, function t: !(is_exception(real_type(t))), function throwexpr: function expr: var wrap_static = mk_static_field_access(hx_exception, "wrap", TFun(::((new Tuple("obj", False, t_dynamic)), []), hx_exception_t), expr.epos);
		{
			(throwexpr) with eexpr = TThrow({
				(expr) with eexpr = TCall(wrap_static, ::(expr, []));
				etype = hx_exception_t
			});
			etype = gen.gcon.basic.tvoid
		}, function v_to_unwrap: function pos: var local = mk_cast(hx_exception_t, {
			() with eexpr = TLocal(v_to_unwrap);
			etype = v_to_unwrap.v_type;
			epos = pos
		});
		mk_field_access(gen, local, "obj", pos), function rethrow: var wrap_static = mk_static_field_access(hx_exception, "wrap", TFun(::((new Tuple("obj", False, t_dynamic)), []), hx_exception_t), rethrow.epos);
		{
			(rethrow) with eexpr = TThrow({
				(rethrow) with eexpr = TCall(wrap_static, ::(rethrow, []));
				etype = hx_exception_t
			})
		}, base_exception_t, hx_exception_t, function v: function e: var exc_cl = get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "Exceptions"))));
		var exc_field = mk_static_field_access_infer(exc_cl, "setException", e.epos, []);
		var esetstack = {
			() with eexpr = TCall(exc_field, ::(mk_local(v, e.epos), []));
			etype = gen.gcon.basic.tvoid;
			epos = e.epos
		};
		Type.concat(esetstack, e)));
		function get_typeof(e) return {
			{
				(e) with eexpr = TCall({
					() with eexpr = TLocal(alloc_var("__typeof__", t_dynamic));
					etype = t_dynamic;
					epos = e.epos
				}, ::(e, []))
			};
		};
		ClassInstance.configure(gen, ClassInstance.traverse(gen, function e: function mt: get_typeof(e)));
		TypeParams.configure(gen, function ecall: function efield: function params: function elist: { (ecall) with eexpr = TCall(efield, elist) });
		CastDetect.configure(gen, CastDetect.default_implementation(gen, native_string_cast = False, Some(TEnum(empty_e, [])), False));
		SwitchToIf.configure(gen, SwitchToIf.traverse(gen, function e:
		switch (e.eexpr) {
	case TSwitch(cond, cases, def): switch (gen.gfollowrun_f(cond.etype)) {
			case TInst({ cl_path = (::(haxe, []), Int32) }, []) | TAbstract({ a_path = ([], Int) }, []) | TInst({ cl_path = ([], String) }, [])
					: List.exists(function (c, _): List.exists(function expr:
				switch (expr.eexpr) {
			case TConst(_): False;
				case _: True;
				}, c), cases);
			case _: True;
			};
		case _: assert False;
		}, True));
		ExpressionUnwrap.configure(gen, ExpressionUnwrap.traverse(gen, function e: Some({ () with eexpr = TVar(mk_temp(gen, "expr", e.etype), Some(e));
								   etype = gen.gcon.basic.tvoid;
								   epos = e.epos
																						})));
		UnnecessaryCastsRemoval.configure(gen);
		IntDivisionSynf.configure(gen, IntDivisionSynf.default_implementation(gen, True));
		UnreachableCodeEliminationSynf.configure(gen, UnreachableCodeEliminationSynf.traverse(gen, False, True, True, True));
		ArrayDeclSynf.configure(gen, ArrayDeclSynf.default_implementation(gen, native_arr_cl));
		var goto_special = alloc_var("__goto__", t_dynamic);
		var label_special = alloc_var("__label__", t_dynamic);
		SwitchBreakSynf.configure(gen, SwitchBreakSynf.traverse(gen, function e_loop: function n: function api: { (e_loop) with eexpr = TBlock(::({ () with eexpr = TCall(mk_local(label_special, e_loop.epos), ::(mk_int(gen, n, e_loop.epos), []));
								  etype = t_dynamic;
								  epos = e_loop.epos
																																				  }, ::(e_loop, [])))
																												}, function e_break: function n: function api: { () with eexpr = TCall(mk_local(goto_special, e_break.epos), ::(mk_int(gen, n, e_break.epos), []));
																														etype = t_dynamic;
																														epos = e_break.epos
																																							   }));
		DefaultArguments.configure(gen, DefaultArguments.traverse(gen));
		InterfaceMetas.configure(gen);
		JavaSpecificSynf.configure(gen, JavaSpecificSynf.traverse(gen, runtime_cl));
		JavaSpecificESynf.configure(gen, JavaSpecificESynf.traverse(gen, runtime_cl));
		var str_cl = switch (gen.gcon.basic.tstring) {
		case TInst(cl, _): cl;
		case _: assert False;
		};
		str_cl.cl_super = Some(get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "NativeString")))), []);
		function mkdir(dir) return {
			if (!(Sys.file_exists(dir))) {
				Unix.mkdir(dir, 0o755);
			} else {
				[];
			};
		};
		mkdir(gen.gcon.file);
		mkdir( ^ (gen.gcon.file, "/src"));
		var out_files = ref([]);
		var res = ref([]);
		Hashtbl.iter(function name: function v: res.val = ::({ () with eexpr = TConst(TString(name));
					 etype = gen.gcon.basic.tstring;
					 epos = Ast.null_pos
															 }, res.val);
					 var name = Codegen.escape_res_name(name, True);
					 var full_path = ^ (gen.gcon.file, ^ ("/src/", name));
					 mkdir_from_path(full_path);
					 var f = open_out_bin(full_path);
					 output_string(f, v);
					 close_out(f);
					 out_files.val = ::(unique_full_path(full_path), out_files.val), gen.gcon.resources);
		try {
			var c = get_cl(Hashtbl.find(gen.gtypes, (new Tuple(::("haxe", []), "Resource"))));
			var cf = PMap.find("content", c.cl_statics);
			cf.cf_expr = Some({
				() with eexpr = TArrayDecl(res.val);
				etype = gen.gcon.basic.tarray(gen.gcon.basic.tstring);
				epos = Ast.null_pos
			});
		} catch (e: Not_found) {
			[];
		};
		run_filters(gen);
		TypeParams.RenameTypeParameters.run(gen);
		var parts = Str.split_delim(Str.regexp("[\\/]+"), gen.gcon.file);
		mkdir_recursive("", parts);
		generate_modules_t(gen, "java", "src", change_path, module_gen, out_files);
		if (!(Common.defined(gen.gcon, Define.KeepOldOutput))) {
			clean_files( ^ (gen.gcon.file, "/src"), out_files.val, gen.gcon.verbose);
		} else {
			[];
		};
		function path_s_desc(path) return {
			path_s(path, []);
		};
		dump_descriptor(gen, "hxjava_build.txt", path_s_desc, function md: path_s_desc(t_infos(md).mt_path));
		if (!(Common.defined(gen.gcon, Define.NoCompilation))) {
			var old_dir = Sys.getcwd([]);
			Sys.chdir(gen.gcon.file);
			var cmd = ^ ("haxelib run hxjava hxjava_build.txt --haxe-version ", ^ (string_of_int(gen.gcon.version),
						 " --feature-level 1"));
			print_endline(cmd);
			if (<>(gen.gcon.run_command(cmd), 0)) {
				failwith("Build failed");
			} else {
				[];
			};
			Sys.chdir(old_dir);
		} else {
			[];
		};
	};

	public static function before_generate(con) return {
		var java_ver = try {
			int_of_string(PMap.find("java_ver", con.defines));
		} catch (e: Not_found) {
			Common.define_value(con, Define.JavaVer, "7");
			7;
		};
		if ( < (java_ver, 5)) {
			failwith( ^ ("Java version is defined to target Java ", ^ (string_of_int(java_ver),
						 ", but the compiler can only output code to versions equal or superior to Java 5")));
		} else {
			[];
		};
		function loop(i) return {
			Common.raw_define(con, ^ ("java", string_of_int(i)));
			if ( > (i, 0)) {
				loop(-(i, 1));
			} else {
				[];
			};
		};
		loop(java_ver);
		[];
	};

	public static function generate(con) return {
		var exists = ref(False);
		con.java_libs = List.map(function (file, std, close, la, gr):
		if (String.ends_with(file, "hxjava-std.jar")) {
		exists.val = True;
		(new Tuple(file, True, close, la, gr));
		} else {
			(new Tuple(file, std, close, la, gr));
		}, con.java_libs);
		if (!(exists.val)) {
			failwith("Your version of hxjava is outdated. Please update it by running: `haxelib update hxjava`");
		} else {
			[];
		};
		var gen = new_ctx(con);
		gen.gallow_tp_dynamic_conversion = True;
		var basic = con.basic;
		var cl_cl = get_cl(get_type(gen, (new Tuple(::("java", ::("lang", [])), "Class"))));
		var basic_fns = ::(mk_class_field("equals", TFun(::((new Tuple("obj", False, t_dynamic)), []), basic.tbool), True, Ast.null_pos, Method(MethNormal), []), ::(mk_class_field("toString", TFun([], basic.tstring), True, Ast.null_pos, Method(MethNormal), []), ::(mk_class_field("hashCode", TFun([], basic.tint), True, Ast.null_pos, Method(MethNormal), []), ::(mk_class_field("getClass", TFun([], TInst(cl_cl, ::(t_dynamic, []))), True, Ast.null_pos, Method(MethNormal), []), ::(mk_class_field("wait", TFun([], basic.tvoid), True, Ast.null_pos, Method(MethNormal), []), ::(mk_class_field("notify", TFun([], basic.tvoid), True, Ast.null_pos, Method(MethNormal), []), ::(mk_class_field("notifyAll", TFun([], basic.tvoid), True, Ast.null_pos, Method(MethNormal), []), [])))))));
		List.iter(function cf: gen.gbase_class_fields = PMap.add(cf.cf_name, cf, gen.gbase_class_fields), basic_fns);
		try {
			configure(gen);
		} catch (e: TypeNotFound(path)) {
			con.error( ^ ("Error. Module '", ^ (path_s(path), "' is required and was not included in build.")), Ast.null_pos);
		};
		debug_mode.val = False;
	};

	public static function error(s, p) return {
		raise(ConversionError(s, p));
	};

	public static function is_haxe_keyword(match) return switch (match) {
	case callback | cast | extern | function | in | typedef | using | var | untyped | inline: True;
	case _: False;
	};

	public static function jname_to_hx(name) return {
		var name = if ( && (<>(name, ""), || (<(String.get(name, 0), 'A'), >(String.get(name, 0), 'Z')))) {
			^ (Char.escaped(Char.uppercase(String.get(name, 0))), String.sub(name, 1, -(String.length(name), 1)));
		} else {
			name;
		};
		var name = String.concat("__", String.nsplit(name, "_"));
	String.map(function case '$': '_';
				   case c: c, name);
	};

	public static function normalize_pack(pack) return {
	List.map(function case : "";
		case str if (&&(>=(String.get(str, 0), 'A'), <=(String.get(str, 0), 'Z'))): String.lowercase(str);
			case str: str, pack);
	};

	public static function jpath_to_hx(Tuple(pack, name)) return {
		switch ((new Tuple(pack, name))) {
		case (::(haxe, ::(root, [])), name): (new Tuple([], name));
		case (::(com, ::(oracle | sun, _)), _) | (::(javax, _), _) | (::(org, ::(ietf | jcp | omg | w3c | xml, _)), _) | (::(sun, _), _) | (::(sunw, _), _)
				: (new Tuple(::("java", normalize_pack(pack)), jname_to_hx(name)));
		case (pack, name): (new Tuple(normalize_pack(pack), jname_to_hx(name)));
		};
	};

	public static function real_java_path(ctx, Tuple(pack, name)) return {
		path_s((new Tuple(pack, name)));
	};

	public static function lookup_jclass(com, path) return {
		var path = jpath_to_hx(path);
		List.fold_right(function (_, _, _, _, get_raw_class): function acc:
		switch (acc) {
	case None: get_raw_class(path);
		case Some(p): Some(p);
		}, com.java_libs, None);
	};

	public static function mk_type_path(ctx, path, params) return {
		var Tuple(name, sub) = try {
			var Tuple(p, _) = String.split(snd(path), "$");
			(new Tuple(jname_to_hx(p), Some(jname_to_hx(snd(path)))));
		} catch (e: Invalid_string) {
			(new Tuple(jname_to_hx(snd(path)), None));
		};
		var pack = fst(jpath_to_hx(path));
		var Tuple(pack, sub, name) = switch (path) {
		case ([], Float = c) | ([], Int = c) | ([], Single = c) | ([], Bool = c) | ([], Dynamic = c) | ([], Iterator = c) | ([], ArrayAccess = c) | ([], Iterable = c)
				: (new Tuple([], Some(c), "StdTypes"));
		case ([], String = c): (new Tuple(::("std", []), None, c));
		case _: (new Tuple(pack, sub, name));
		};
		CTPath({ () with tpackage = pack;
				 tname = name;
				 tparams = params;
				 tsub = sub
			   });
	};

	public static function has_tparam(name, params) return {
		List.exists(function (n, _, _): = (n, name), params);
	};

	public static function convert_arg(ctx, p, arg) return {
		switch (arg) {
		case TAny | TType(WSuper, _): TPType(mk_type_path(ctx, (new Tuple([], "Dynamic")), []));
		case TType(_, jsig): TPType(convert_signature(ctx, p, jsig));
		};
	};

	public static function convert_signature(ctx, p, jsig) return {
		switch (jsig) {
		case TByte: mk_type_path(ctx, (new Tuple(::("java", ::("types", [])), "Int8")), []);
		case TChar: mk_type_path(ctx, (new Tuple(::("java", ::("types", [])), "Char16")), []);
		case TDouble: mk_type_path(ctx, (new Tuple([], "Float")), []);
		case TFloat: mk_type_path(ctx, (new Tuple([], "Single")), []);
		case TInt: mk_type_path(ctx, (new Tuple([], "Int")), []);
		case TLong: mk_type_path(ctx, (new Tuple(::("haxe", []), "Int64")), []);
		case TShort: mk_type_path(ctx, (new Tuple(::("java", ::("types", [])), "Int16")), []);
		case TBool: mk_type_path(ctx, (new Tuple([], "Bool")), []);
		case TObject((::(haxe, ::(root, [])), name), args): mk_type_path(ctx, (new Tuple([], name)), List.map(convert_arg(ctx, p),
			args));
		case TObject((::(java, ::(lang, [])), Object), []): mk_type_path(ctx, (new Tuple([], "Dynamic")), []);
		case TObject((::(java, ::(lang, [])), String), []): mk_type_path(ctx, (new Tuple([], "String")), []);
		case TObject((::(java, ::(lang, [])), Enum), ::(_, [])): mk_type_path(ctx, (new Tuple([], "EnumValue")), []);
		case TObject(path, []): switch (lookup_jclass(ctx.jcom, path)) {
			case Some(jcl, _, _): mk_type_path(ctx, path, List.map(function _: convert_arg(ctx, p, TAny), jcl.ctypes));
			case None: mk_type_path(ctx, path, []);
			};
		case TObject(path, args): mk_type_path(ctx, path, List.map(convert_arg(ctx, p), args));
		case TObjectInner(pack, ::((name, params), inners)): var actual_param = switch (List.rev(inners)) {
			case ::((_, p), _): p;
			case _: assert False;
			};
			mk_type_path(ctx, (new Tuple(pack, ^ (name, ^ ("$", String.concat("$", List.map(fst, inners)))))),
						 List.map(function param: convert_arg(ctx, p, param), actual_param));
		case TObjectInner(pack, inners): assert False;
		case TArray(jsig, _): mk_type_path(ctx, (new Tuple(::("java", []), "NativeArray")), ::(TPType(convert_signature(ctx, p,
											   jsig)), []));
		case TMethod(_): JReader.error("TMethod cannot be converted directly into Complex Type");
		case TTypeParameter(s): switch (ctx.jtparams) {
			case ::(cur, others): if (has_tparam(s, cur)) {
					mk_type_path(ctx, (new Tuple([], s)), []);
				} else {
					if ( && (ctx.jcom.verbose, !(List.exists(has_tparam(s), others)))) {
						print_endline( ^ ("Type parameter ", ^ (s, " was not found while building type!")));
					} else {
						[];
					};
					mk_type_path(ctx, (new Tuple([], "Dynamic")), []);
				};
			case _: if (ctx.jcom.verbose) {
					print_endline("Empty type parameter stack!");
				} else {
					[];
				};
				mk_type_path(ctx, (new Tuple([], "Dynamic")), []);
			};
		};
	};

	public static function convert_constant(ctx, p, const) return {
	Option.map_default(function case ConstString(s): Some(EConst(String(s)), p);
		case ConstInt(i): Some(EConst(Int(Printf.sprintf("%ld", i))), p);
			case ConstFloat(f) | ConstDouble(f): Some(EConst(Float(Printf.sprintf("%E", f))), p);
				case _: None, None, const);
	};

	public static function same_sig(parent, jsig) return {
		switch (jsig) {
		case TObject(p, targs): || ( = (parent, p), List.exists(function case TType(_, s): same_sig(parent, s);
			case _: False, targs));
		case TObjectInner(p, ntargs):
			|| ( = (parent, (new Tuple(p, String.concat("$", List.map(fst, ntargs))))), List.exists(function (_,
		targs): List.exists(function case TType(_, s): same_sig(parent, s);
			case _: False, targs), ntargs));
		case TArray(s, _): same_sig(parent, s);
		case _: False;
		};
	};

	public static function convert_param(ctx, p, parent, param) return {
		var Tuple(name, constraints) = switch (param) {
		case (name, Some(extends_sig), implem_sig): (new Tuple(name, ::(extends_sig, implem_sig)));
		case (name, None, implemem_sig): (new Tuple(name, implemem_sig));
		};
		var constraints = List.map(function s:
		if (same_sig(parent, s)) {
		TObject((new Tuple(::("java", ::("lang", [])), "Object")), []);
		} else {
			s;
		}, constraints);
		{
			() with tp_name = name;
			tp_params = [];
			tp_constraints = List.map(convert_signature(ctx, p), constraints);
			tp_meta = []
		};
	};

	public static function get_type_path(ctx, ct) return {
		switch (ct) {
		case CTPath(p): p;
		case _: assert False;
		};
	};

	public static function is_override(field) return {
	List.exists(function case AttrVisibleAnnotations(::({ ann_type = TObject((::(java, ::(lang, [])), Override), _) }, [])): True;
	case _: False, field.jf_attributes);
	};

	public static function mk_override(field) return {
		{
			(field) with jf_attributes = ::(AttrVisibleAnnotations(::({
				() with ann_type = TObject((new Tuple(::("java", ::("lang", [])), "Override")), []);
				ann_elements = []
			}, [])), field.jf_attributes)
		};
	};

	public static function del_override(field) return {
		{ (field) with jf_attributes = List.filter(function a: !(is_override_attrib(a)), field.jf_attributes) };
	};

	public static function get_canonical(ctx, p, pack, name) return {
		(new Tuple(Meta.JavaCanonical, ::((new Tuple(EConst(String(String.concat(".", pack))), p)), ::((new Tuple(EConst(String(name)), p)), [])), p));
	};

	public static function convert_java_enum(ctx, p, pe) return {
		var meta = ref(::(get_canonical(ctx, p, fst(pe.cpath), snd(pe.cpath)), ::((new Tuple(Meta.Native, ::((new Tuple(EConst(String(real_java_path(ctx, pe.cpath))), p)), []), p)), [])));
		var data = ref([]);
		List.iter(function f:
		switch (f.jf_vmsignature) {
	case TObject(path, []) if (&&(=(path, pe.cpath), &&(List.mem(JStatic, f.jf_flags), List.mem(JFinal, f.jf_flags)))):
			data.val = ::({
				() with ec_name = f.jf_name;
				ec_doc = None;
				ec_meta = [];
				ec_args = [];
				ec_pos = p;
				ec_params = [];
				ec_type = None
			}, data.val);
		case _: [];
		}, pe.cfields);
		EEnum({ () with d_name = jname_to_hx(snd(pe.cpath));
				d_doc = None;
				d_params = [];
				d_meta = meta.val;
				d_flags = ::(EExtern, []);
				d_data = List.rev(data.val)
			  });
	};

	public static function convert_java_field(ctx, p, jc, field) return {
		var p = { (p) with pfile = ^ (p.pfile, ^ (", [", ^ (field.jf_name, "]"))) };
		var cff_doc = None;
		var cff_pos = p;
		var cff_meta = ref([]);
		var cff_access = ref([]);
		var cff_name = switch (field.jf_name) {
		case <init>: "new";
		case <clinit>: raise(Exit);
		case name if (>(String.length(name), 5)):
			switch (String.sub(name, 0, 5)) {
			case __hx_ | this$: raise(Exit);
			case _: name;
			};
		case name: name;
		};
		var jf_constant = ref(field.jf_constant);
		var readonly = ref(False);
	List.iter(function case JPublic: cff_access.val = ::(APublic, cff_access.val);
				  case JPrivate: raise(Exit);
					  case JProtected: cff_access.val = ::(APrivate, cff_access.val);
						  case JStatic: cff_access.val = ::(AStatic, cff_access.val);
							  case JFinal: cff_meta.val = ::((new Tuple(Meta.Final, [], p)), cff_meta.val);
							switch ((new Tuple(field.jf_kind, field.jf_vmsignature, field.jf_constant))) {
						case (JKField, TObject(_), _): jf_constant.val = None;
							case (JKField, _, Some(_)): readonly.val = True;
								jf_constant.val = None;
							case _: jf_constant.val = None;
							};
	case JVolatile: cff_meta.val = ::((new Tuple(Meta.Volatile, [], p)), cff_meta.val);
									   case JTransient: cff_meta.val = ::((new Tuple(Meta.Transient, [], p)), cff_meta.val);
										   case _: [], field.jf_flags);
	List.iter(function case AttrDeprecated if (<>(jc.cpath, (new Tuple(::("java", ::("util", [])), "Date")))): cff_meta.val = ::((new Tuple(Meta.Deprecated, [], p)), cff_meta.val);
		case AttrVisibleAnnotations(ann): List.iter(function case { ann_type = TObject((::(java, ::(lang, [])), Override), []) }: cff_access.val = ::(AOverride, cff_access.val);
			case _: [], ann);
		case _: [], field.jf_attributes);
		List.iter(function jsig:
		switch (convert_signature(ctx, p, jsig)) {
	case CTPath(path): cff_meta.val = ::((new Tuple(Meta.Throws,
												  ::((new Tuple(Ast.EConst(Ast.String(path_s((new Tuple(path.tpackage, path.tname))))), p)), []), p)), cff_meta.val);
		case _: [];
		}, field.jf_throws);
		var kind = switch (field.jf_kind) {
		case JKField if (readonly.val): FProp("default", "null", Some(convert_signature(ctx, p, field.jf_signature)), None);
		case JKField: FVar(Some(convert_signature(ctx, p, field.jf_signature)), None);
		case JKMethod: switch (field.jf_signature) {
			case TMethod(args, ret): var old_types = ctx.jtparams;
				switch (ctx.jtparams) {
				case ::(c, others): ctx.jtparams = ::(@(c, field.jf_types), others);
				case []: ctx.jtparams = ::(field.jf_types, []);
				};
				var i = ref(0);
				var args = List.map(function s: incr(i);
									(new Tuple( ^ ("param", string_of_int(i.val)), False, Some(convert_signature(ctx, p, s)), None)), args);
				var t = Option.map_default(convert_signature(ctx, p), mk_type_path(ctx, (new Tuple([], "Void")), []), ret);
				cff_meta.val = ::((new Tuple(Meta.Overload, [], p)), cff_meta.val);
			var types = List.map(function case (name, Some(ext), impl): {
										 () with tp_name = name;
										 tp_params = [];
										 tp_constraints = List.map(convert_signature(ctx, p), ::(ext, impl));
										 tp_meta = []
									 };
									 case (name, None, impl): {
											 () with tp_name = name;
											 tp_params = [];
											 tp_constraints = List.map(convert_signature(ctx, p), impl);
											 tp_meta = []
										 }, field.jf_types);
			ctx.jtparams = old_types;
			FFun({ () with f_params = types;
				   f_args = args;
				   f_type = Some(t);
				   f_expr = None
				 });
			case _: error("Method signature was expected", p);
			};
		};
		var Tuple(cff_name, cff_meta) = switch (String.get(cff_name, 0)) {
		case '%': var name = String.sub(cff_name, 1, -(String.length(cff_name), 1));
			if (!(is_haxe_keyword(name))) {
				cff_meta.val = ::((new Tuple(Meta.Deprecated, ::((new Tuple(EConst(String( ^ ("This static field `_", ^ (name,
											 ^ ("` is deprecated and will be removed in later versions. Please use `", ^ (name, "` instead")))))), p)), []), p)),
								  cff_meta.val);
			} else {
				[];
			};
			(new Tuple( ^ ("_", name), ::((new Tuple(Meta.Native, ::((new Tuple(EConst(String(name)), cff_pos)), []), cff_pos)),
										  cff_meta.val)));
		case _: switch (String.nsplit(cff_name, "$")) {
			case ::(no_dollar, []): (new Tuple(cff_name, cff_meta.val));
			case parts: (new Tuple(String.concat("_", parts), ::((new Tuple(Meta.Native, ::((new Tuple(EConst(String(cff_name)),
									   cff_pos)), []), cff_pos)), cff_meta.val)));
			};
		};
		if (PMap.mem("java_loader_debug", ctx.jcom.defines)) {
			Printf.printf("\t%s%sfield %s : %s\n", if (List.mem(AStatic, cff_access.val)) {
			"static ";
		} else {
			"";
		}, if (List.mem(AOverride, cff_access.val)) {
			"override ";
		} else {
			"";
		}, cff_name, s_sig(field.jf_signature));
		} else {
			[];
		};
		{
			() with cff_name = cff_name;
			cff_doc = cff_doc;
			cff_pos = cff_pos;
			cff_meta = cff_meta;
			cff_access = cff_access.val;
			cff_kind = kind
		};
	};

	public static function japply_params(params, jsig) return {
		switch (params) {
		case []: jsig;
		case _: switch (jsig) {
			case TTypeParameter(s): try {
					List.assoc(s, params);
				} catch (e: Not_found) {
					jsig;
				};
			case TObject(p, tl): TObject(p, args(params, tl));
			case TObjectInner(sl, stll): TObjectInner(sl, List.map(function (s, tl): (new Tuple(s, args(params, tl))), stll));
			case TArray(s, io): TArray(japply_params(params, s), io);
			case TMethod(sl, sopt): TMethod(List.map(japply_params(params), sl), Option.map(japply_params(params), sopt));
			case _: jsig;
			};
		};
	};

	public static function args(params, tl) return {
		switch (params) {
		case []: tl;
		case _: List.map(function case TAny: TAny;
			case TType(w, s): TType(w, japply_params(params, s)), tl);
		};
	};

	public static function mk_params(jtypes) return {
		List.map(function (s, _, _): (new Tuple(s, TTypeParameter(s))), jtypes);
	};

	public static function convert_java_class(ctx, p, jc) return {
		switch (List.mem(JEnum, jc.cflags)) {
		case True: ::(convert_java_enum(ctx, p, jc), []);
		case False: var flags = ref(::(HExtern, []));
			if (PMap.mem("java_loader_debug", ctx.jcom.defines)) {
				var sup = ::(jc.csuper, jc.cinterfaces);
				print_endline( ^ ("converting ", ^ (if (List.mem(JAbstract, jc.cflags)) {
				"abstract ";
			} else {
				"";
			}, ^ (JData.path_s(jc.cpath), ^ (" : ", String.concat(", ", List.map(s_sig, sup)))))));
			} else {
				[];
			};
			var meta = ref(::((new Tuple(Meta.JavaNative, [], p)), ::((new Tuple(Meta.Native,
							  ::((new Tuple(EConst(String(real_java_path(ctx, jc.cpath))), p)), []), p)), ::(get_canonical(ctx, p, fst(jc.cpath),
									  snd(jc.cpath)), []))));
			var force_check = Common.defined(ctx.jcom, Define.ForceLibCheck);
			if (!(force_check)) {
				meta.val = ::((new Tuple(Meta.LibType, [], p)), meta.val);
			} else {
				[];
			};
			var is_interface = ref(False);
			List.iter(function f:
			switch (f) {
		case JFinal: meta.val = ::((new Tuple(Meta.Final, [], p)), meta.val);
			case JInterface: is_interface.val = True;
				flags.val = ::(HInterface, flags.val);
			case JAbstract: meta.val = ::((new Tuple(Meta.Abstract, [], p)), meta.val);
			case JAnnotation: meta.val = ::((new Tuple(Meta.Annotation, [], p)), meta.val);
			case _: [];
			}, jc.cflags);
			switch (jc.csuper) {
			case TObject((::(java, ::(lang, [])), Object), _): [];
			case TObject((::(haxe, ::(lang, [])), HxObject), _): meta.val = ::((new Tuple(Meta.HxGen, [], p)), meta.val);
			case _: flags.val = ::(HExtends(get_type_path(ctx, convert_signature(ctx, p, jc.csuper))), flags.val);
			};
			List.iter(function i:
			switch (i) {
		case TObject((::(haxe, ::(lang, [])), IHxObject), _): meta.val = ::((new Tuple(Meta.HxGen, [], p)), meta.val);
			case _: flags.val = if (is_interface.val) {
					::(HExtends(get_type_path(ctx, convert_signature(ctx, p, i))), flags.val);
				} else {
					::(HImplements(get_type_path(ctx, convert_signature(ctx, p, i))), flags.val);
				};
			}, jc.cinterfaces);
			var fields = ref([]);
			var jfields = ref([]);
			if (<>(jc.cpath, (new Tuple(::("java", ::("lang", [])), "CharSequence")))) {
				List.iter(function f:
				try {
					if ( && (is_interface.val, List.mem(JStatic, f.jf_flags))) {
						[];
					} else {
						fields.val = ::(convert_java_field(ctx, p, jc, f), fields.val);
						jfields.val = ::(f, jfields.val);
					};
				} catch (e: Exit) {
					[];
				}, @(jc.cfields, jc.cmethods));
			} else {
				[];
			};
			var imports = List.concat(List.map(function f: List.map(function jsig:
			switch (convert_signature(ctx, p, jsig)) {
		case CTPath(path): var pos = { (p) with pfile = ^ (p.pfile, ^ (", [", ^ (f.jf_name, " @:throws]"))) };
				EImport(List.map(function s: (new Tuple(s, pos)), @(path.tpackage, ::(path.tname, []))), INormal);
			case _: assert False;
			}, f.jf_throws), jc.cmethods));
			::(EClass({ () with d_name = jname_to_hx(snd(jc.cpath));
						d_doc = None;
						d_params = List.map(convert_param(ctx, p, jc.cpath), jc.ctypes);
						d_meta = meta.val;
						d_flags = flags.val;
						d_data = fields.val
					  }), imports);
		};
	};

	public static function create_ctx(com) return {
		{
			() with jcom = com;
			jtparams = []
		};
	};

	public static function has_type_param(match) return switch (match) {
	case TTypeParameter(_): True;
	case TMethod(lst, opt): || (List.exists(has_type_param, lst), Option.map_default(has_type_param, False, opt));
	case TArray(s, _): has_type_param(s);
	case TObjectInner(_, stpl): List.exists(function (_, sigs): List.exists(has_type_param_arg, sigs), stpl);
	case TObject(_, pl): List.exists(has_type_param_arg, pl);
	case _: False;
	};

	public static function has_type_param_arg(match) return switch (match) {
	case TType(_, s): has_type_param(s);
	case _: False;
	};

	public static function japply_params(jparams, jsig) return {
		switch (jparams) {
		case []: jsig;
		case _: switch (jsig) {
			case TObject(path, p): TObject(path, List.map(japply_params_tp(jparams), p));
			case TObjectInner(sl, stargl): TObjectInner(sl, List.map(function (s, targ): (new Tuple(s,
				List.map(japply_params_tp(jparams), targ))), stargl));
			case TArray(jsig, io): TArray(japply_params(jparams, jsig), io);
			case TMethod(args, ret): TMethod(List.map(japply_params(jparams), args), Option.map(japply_params(jparams), ret));
			case TTypeParameter(s): try {
					List.assoc(s, jparams);
				} catch (e: Not_found) {
					jsig;
				};
			case _: jsig;
			};
		};
	};

	public static function japply_params_tp(jparams, jtype_argument) return {
		switch (jtype_argument) {
		case TAny: TAny;
		case TType(w, jsig): TType(w, japply_params(jparams, jsig));
		};
	};

	public static function mk_jparams(jtypes, params) return {
		switch ((new Tuple(jtypes, params))) {
		case ([], []): [];
		case (_, []): List.map(function (s, _, _): (new Tuple(s, TObject((new Tuple(::("java", ::("lang", [])), "Object")), []))),
			jtypes);
		case _: List.map2(function (s, _, _): function jt: switch (jt) {
		case TAny: (new Tuple(s, TObject((new Tuple(::("java", ::("lang", [])), "Object")), [])));
			case TType(_, jsig): (new Tuple(s, jsig));
			}, jtypes, params);
		};
	};

	public static function compatible_signature_arg(arg_test, f1, f2) return {
		var arg_test = switch (arg_test) {
		case None: function _: function _: True;
		case Some(a): a;
		};
		if ( = (f1, f2)) {
			True;
		} else {
			switch ((new Tuple(f1, f2))) {
			case (TObject(p, a), TObject(p2, a2)): && ( = (p, p2), arg_test(a, a2));
			case (TObjectInner(sl, stal), TObjectInner(sl2, stal2)): && ( = (sl, sl2), = (List.map(fst, stal), List.map(fst, stal2)));
			case (TArray(s, _), TArray(s2, _)): compatible_signature_arg(s, s2);
			case (TTypeParameter(t1), TTypeParameter(t2)): = (t1, t2);
			case _: False;
			};
		};
	};

	public static function compatible_param(p1, p2) return {
		switch ((new Tuple(p1, p2))) {
		case (TType(_, s1), TType(_, s2)): compatible_signature_arg(arg_test = compatible_tparams, s1, s2);
		case (TAny, TType(_, TObject((::(java, ::(lang, [])), Object), _))): True;
		case (TType(_, TObject((::(java, ::(lang, [])), Object), _)), TAny): True;
		case _: False;
		};
	};

	public static function compatible_tparams(p1, p2) return {
		try {
			switch ((new Tuple(p1, p2))) {
			case ([], []): True;
			case (_, []): var p2 = List.map(function _: TAny, p1);
				List.for_all2(compatible_param, p1, p2);
			case ([], _): var p1 = List.map(function _: TAny, p2);
				List.for_all2(compatible_param, p1, p2);
			case (_, _): List.for_all2(compatible_param, p1, p2);
			};
		} catch (e: Invalid_argument(List.for_all2)) {
			False;
		};
	};

	public static function get_adapted_sig(f, f2) return {
		switch (f.jf_types) {
		case []: f.jf_signature;
		case _: var jparams = mk_jparams(f.jf_types, List.map(function (s, _, _): TType(WNone, TTypeParameter(s)), f2.jf_types));
			japply_params(jparams, f.jf_signature);
		};
	};

	public static function compatible_methods(f1, f2) return {
		if (<>(List.length(f1.jf_types), List.length(f2.jf_types))) {
			False;
		} else {
			switch ((new Tuple(get_adapted_sig(f1, f2), f2.jf_signature))) {
			case (TMethod(a1, _), TMethod(a2, _)) if (=(List.length(a1), List.length(a2))): List.for_all2(compatible_signature_arg, a1,
				a2);
			case _: False;
			};
		};
	};

	public static function jcl_from_jsig(com, jsig) return {
		var Tuple(path, params) = switch (jsig) {
		case TObject(path, params): (new Tuple(path, params));
		case TObjectInner(sl, stll): var last_params = ref([]);
			var real_path = (new Tuple(sl, String.concat("$", List.map(function (s, p): last_params.val = p;
			s, stll))));
			(new Tuple(real_path, last_params.val));
		case _: raise(Not_found);
		};
		switch (lookup_jclass(com, path)) {
		case None: raise(Not_found);
		case Some(c, _, _): (new Tuple(c, params));
		};
	};

	public static function jclass_with_params(com, cls, params) return {
		try {
			switch (cls.ctypes) {
			case []: cls;
			case _: var jparams = mk_jparams(cls.ctypes, params);
				{
					(cls) with cfields = List.map(function f: {
						(f) with jf_signature = japply_params(jparams, f.jf_signature)
					}, cls.cfields);
					cmethods = List.map(function f: {
											(f) with jf_signature = japply_params(jparams, f.jf_signature)
										}, cls.cmethods);
					csuper = japply_params(jparams, cls.csuper);
					cinterfaces = List.map(japply_params(jparams), cls.cinterfaces)
				};
			};
		} catch (e: Invalid_argument(List.map2)) {
			if (com.verbose) {
				prerr_endline( ^ ("Differing parameters for class: ", path_s(cls.cpath)));
			} else {
				[];
			};
			cls;
		};
	};

	public static function is_object(match) return switch (match) {
	case TObject((::(java, ::(lang, [])), Object), []): True;
	case _: False;
	};

	public static function is_tobject(match) return switch (match) {
	case TObject(_) | TObjectInner(_): True;
	case _: False;
	};

	public static function simplify_args(args) return {
	if (List.for_all(function case TAny: True;
		case _: False, args)) {
					[];
				} else {
			args;
		};
	};

	public static function compare_type(com, s1, s2) return {
		if ( = (s1, s2)) {
			0;
		} else {
			if (!(is_tobject(s1))) {
				if (is_tobject(s2)) {
					1;
				} else {
					if (compatible_signature_arg(s1, s2)) {
						0;
					} else {
						raise(Exit);
					};
				};
			} else {
				if (!(is_tobject(s2))) {
					-1;
				} else {
					function loop( ? : (first_error = True), s1, s2) return {
						if (is_object(s1)) {
							= (s1, s2);
						} else {
							if (compatible_signature_arg(s1, s2)) {
								var Tuple(p1, p2) = switch ((new Tuple(s1, s2))) {
								case (TObject(_, p1), TObject(_, p2)): (new Tuple(p1, p2));
								case (TObjectInner(_, npl1), TObjectInner(_, npl2)): (new Tuple(snd(List.hd(List.rev(npl1))),
									snd(List.hd(List.rev(npl2)))));
								case _: assert False;
								};
								var Tuple(p1, p2) = (new Tuple(simplify_args(p1), simplify_args(p2)));
								var lp1 = List.length(p1);
								var lp2 = List.length(p2);
								if ( > (lp1, lp2)) {
									True;
								} else {
									if ( > (lp2, lp1)) {
										False;
									} else {
										if (!(compatible_tparams(p1, p2))) {
											raise(Exit);
										} else {
											[];
										};
										True;
									};
								};
							} else {
								try {
									var Tuple(c, p) = jcl_from_jsig(com, s1);
									var jparams = mk_jparams(c.ctypes, p);
									var super = japply_params(jparams, c.csuper);
									var implements = List.map(japply_params(jparams), c.cinterfaces);
									|| (loop(first_error = first_error, super, s2), List.exists(function super: loop(first_error = first_error, super, s2), implements));
								} catch (e: Not_found) {
									prerr_endline( ^ ("-java-lib: The type ", ^ (s_sig(s1),
									" is referred but was not found. Compilation may not occur correctly.")));
									prerr_endline("Did you forget to include a needed lib?");
									if (first_error) {
										!(loop(first_error = False, s2, s1));
									} else {
										False;
									};
								};
							};
						} : bool;
					};
					if (loop(s1, s2)) {
						if (loop(s2, s1)) {
							0;
						} else {
							1;
						};
					} else {
						if (loop(s2, s1)) {
							-1;
						} else {
							-2;
						};
					};
				};
			};
		};
	};

	public static function select_best(com, flist) return {
		function loop(cur_best) return {
		case []: Some(cur_best);
		case ::(f, flist): switch ((new Tuple(get_adapted_sig(f, cur_best), cur_best.jf_signature))) {
			case (TMethod(_, Some(r)), TMethod(_, Some(r2))): try {
					switch (compare_type(com, r, r2)) {
					case 0: loop(cur_best, flist);
					case 1: loop(f, flist);
					case -1: loop(cur_best, flist);
					case -2: if (com.verbose) {
							prerr_endline( ^ (f.jf_name, ^ (": The types ", ^ (s_sig(r), ^ (" and ", ^ (s_sig(r2), " are incompatible"))))));
						} else {
							[];
						};
						loop(cur_best, flist);
					case _: assert False;
					};
				} catch (e: Exit) {
					if (com.verbose) {
						prerr_endline( ^ (f.jf_name, ^ (": Incompatible argument return signatures: ", ^ (s_sig(r), ^ (" and ", s_sig(r2))))));
					} else {
						[];
					};
					None;
				};
			case (TMethod(_), _): loop(f, flist);
			case _: loop(cur_best, flist);
			};
		};
		switch (loop(List.hd(flist), List.tl(flist))) {
		case Some(f): Some(f);
		case None: switch (List.filter(function f: !(is_override(f)), flist)) {
			case []: None;
			case ::(f, []): Some(f);
			case ::(f, flist): Some(f);
			};
		};
	};

	public static function fix_overrides_jclass(com, cls) return {
		var force_check = Common.defined(com, Define.ForceLibCheck);
		var methods = if (force_check) {
			List.map(function f: del_override(f), cls.cmethods);
		} else {
			cls.cmethods;
		};
		var cmethods = methods;
		var super_fields = [];
		var super_methods = [];
		var nonstatics = List.filter(function f: !(List.mem(JStatic, f.jf_flags)), @(cls.cfields, cls.cmethods));
		function is_pub(f) return {
		List.exists(function case JPublic | JProtected: True;
			case _: False, f.jf_flags);
		};
		var Tuple(cmethods, super_fields) = if (!(List.mem(JInterface, cls.cflags))) {
			(new Tuple(List.filter(is_pub, cmethods), List.filter(is_pub, super_fields)));
		} else {
			(new Tuple(cmethods, super_fields));
		};
		function loop(cls, super_methods, super_fields, cmethods, nonstatics) return {
			try {
				switch (cls.csuper) {
				case TObject((::(java, ::(lang, [])), Object), _): (new Tuple(super_methods, super_fields, cmethods, nonstatics));
				case _: var Tuple(cls, params) = jcl_from_jsig(com, cls.csuper);
					var cls = jclass_with_params(com, cls, params);
					var nonstatics = @(List.filter(function f: List.mem(JStatic, f.jf_flags), @(cls.cfields, cls.cmethods)), nonstatics);
					var super_methods = @(cls.cmethods, super_methods);
					var super_fields = @(cls.cfields, super_fields);
					var cmethods = if (force_check) {
						var overriden = ref([]);
						var cmethods = List.map(function jm:
						if ( && (!(is_override(jm)), && (!(List.mem(JStatic, jm.jf_flags)), List.exists(function msup: var ret =
						&& ( = (msup.jf_name, jm.jf_name), && (!(List.mem(JStatic, msup.jf_flags)), compatible_methods(msup, jm)));
						if (ret) {
							var f = mk_override(msup);
								overriden.val = ::({ (f) with jf_flags = jm.jf_flags }, overriden.val);
							} else {
								[];
							};
						ret, cls.cmethods)))) {
							mk_override(jm);
						} else {
							jm;
						}, cmethods);
						@(overriden.val, cmethods);
					} else {
						cmethods;
					};
					loop(cls, super_methods, super_fields, cmethods, nonstatics);
				};
			} catch (e: Not_found) {
				(new Tuple(super_methods, super_fields, cmethods, nonstatics));
			};
		};
		loop(cls, super_methods, super_fields, cmethods, nonstatics);
	};

	public static function normalize_jclass(com, cls) return {
		var force_check = Common.defined(com, Define.ForceLibCheck);
		var Tuple(super_methods, super_fields, cmethods, nonstatics) = fix_overrides_jclass(com, cls);
		var all_methods = @(cmethods, super_methods);
		var added_interface_fields = ref([]);
		function loop_interface(abstract, cls, iface) return {
			try {
				switch (iface) {
				case TObject((::(java, ::(lang, [])), Object), _): [];
				case TObject(path, _) if (=(path, cls.cpath)): [];
				case _: var Tuple(cif, params) = jcl_from_jsig(com, iface);
					var cif = jclass_with_params(com, cif, params);
					List.iter(function jf:
					if ( && (!(List.mem(JStatic, jf.jf_flags)), !(List.exists(function jf2: && ( = (jf.jf_name, jf2.jf_name),
					&& (!(List.mem(JStatic, jf2.jf_flags)), = (jf.jf_signature, jf2.jf_signature))), all_methods)))) {
					var jf = if ( && (abstract, force_check)) {
							del_override(jf);
						} else {
							jf;
						};
						var jf = { (jf) with jf_flags = ::(JPublic, jf.jf_flags) };
						added_interface_fields.val = ::(jf, added_interface_fields.val);
					} else {
						[];
					}, cif.cmethods);
					if (abstract) {
						List.iter(loop_interface(abstract, cif), cif.cinterfaces);
					} else {
						[];
					};
				};
			} catch (e: Not_found) {
				[];
			};
		};
		List.iter(loop_interface(List.mem(JAbstract, cls.cflags), cls), cls.cinterfaces);
		var nonstatics = @(added_interface_fields.val, nonstatics);
		var cmethods = @(added_interface_fields.val, cmethods);
		var cmethods = if (!(force_check)) {
			cmethods;
		} else {
			List.fold_left(function cmethods: function im: var f = List.find_all(function jf: && ( = (jf.jf_name, im.jf_name), compatible_methods(jf, im)), super_methods);
			var f = List.map(mk_override, f);
			@(f, cmethods), cmethods, added_interface_fields.val);
		};
		var cmethods = if (List.mem(JInterface, cls.cflags)) {
			List.filter(function jf:
			switch ((new Tuple(jf.jf_name, jf.jf_vmsignature))) {
		case (equals, TMethod(::(TObject((::(java, ::(lang, [])), Object), _), []), _)) | (hashCode, TMethod([], _)) | (toString, TMethod([], _))
					: False;
			case _: True;
			}, cmethods);
		} else {
			cmethods;
		};
		function fold_field(acc, f) return {
			var Tuple(change, both) = switch (f.jf_name) {
			case _ if (&&(List.mem(JStatic, f.jf_flags), List.exists(function f2: = (f.jf_name, f2.jf_name),
					nonstatics))): (new Tuple(True, True));
			case _: (new Tuple(is_haxe_keyword(f.jf_name), False));
			};
			var f2 = if (change) {
				{
					(f) with jf_name = ^ ("%", f.jf_name)
				};
			} else {
				f;
			};
			if (both) {
				::(f, ::(f2, acc));
			} else {
				::(f2, acc);
			};
		};
		var cfields = List.fold_left(fold_field, [], cls.cfields);
		var cmethods = List.fold_left(fold_field, [], cmethods);
		function filter_field(f, f2) return {
			&& ( != (f, f2), && ( = (List.mem(JStatic, f.jf_flags), List.mem(JStatic, f2.jf_flags)), && ( = (f.jf_name, f2.jf_name), <>(f2.jf_kind, f.jf_kind))));
		};
		var cfields = List.filter(function f:
		if (List.mem(JStatic, f.jf_flags)) {
		!(List.exists(filter_field(f), cmethods));
		} else {
			&& (!(List.exists(filter_field(f), nonstatics)), !(List.exists(function f2: && ( != (f, f2), && ( = (f.jf_name, f2.jf_name), !(List.mem(JStatic, f2.jf_flags)))), super_fields)));
		}, cfields);
		var cmethods = if (force_check) {
			List.filter(function f:
			if (List.mem(JStatic, f.jf_flags)) {
			True;
		} else {
			!(List.exists(filter_field(f), super_fields));
			}, cmethods);
		} else {
			cmethods;
		};
		function loop(acc) return {
		case []: acc;
		case ::(f, cmeths): switch (List.partition(function f2: && ( = (f.jf_name, f2.jf_name), compatible_methods(f, f2)), cmeths)) {
			case ([], cmeths): loop(::(f, acc), cmeths);
			case (flist, cmeths): switch (select_best(com, ::(f, flist))) {
				case None: loop(acc, cmeths);
				case Some(f): loop(::(f, acc), cmeths);
				};
			};
		};
		var cfields = List.filter(function f: List.exists(function f: || ( = (f, JPublic), = (f, JProtected)), f.jf_flags), cfields);
		var cmethods = loop([], cmethods);
		{
			(cls) with cfields = cfields;
			cmethods = cmethods
		};
	};

	public static function get_classes_zip(zip) return {
		var ret = ref([]);
	List.iter(function case { Zip.is_directory = False; Zip.filename = f } if (&&(=(String.sub(String.uncapitalize(f), -(String.length(f), 6), 6), ".class"), !(String.exists(f, "$")))):
		switch (List.rev(String.nsplit(f, "/"))) {
		case ::(clsname, pack): if (!(String.contains(clsname, '$'))) {
					var path = jpath_to_hx((new Tuple(List.rev(pack), String.sub(clsname, 0, -(String.length(clsname), 6)))));
					ret.val = ::(path, ret.val);
				} else {
					[];
				};
			case _: ret.val = ::((new Tuple([], jname_to_hx(f))), ret.val);
			};
	case _: [], Zip.entries(zip));
		ret.val;
	};

	public static function add_java_lib(com, file, std) return {
		var file = if (Sys.file_exists(file)) {
			file;
		} else {
			try {
				Common.find_file(com, file);
			} catch (e: Not_found) {
				try {
					Common.find_file(com, ^ (file, ".jar"));
				} catch (e: Not_found) {
					failwith( ^ ("Java lib ", ^ (file, " not found")));
				};
			};
		};
		var hxpack_to_jpack = Hashtbl.create(16);
		var Tuple(get_raw_class, close, list_all_files) = switch (Unix.stat(file).st_kind) {
		case S_DIR: var all = ref([]);
			function iter_files(pack, dir, path) return {
				try {
					var file = Unix.readdir(dir);
					var filepath = ^ (path, ^ ("/", file));
					if ( && (String.ends_with(file, ".class"), !(String.exists(file, "$")))) {
						var file = String.sub(file, 0, -(String.length(file), 6));
						var path = jpath_to_hx((new Tuple(pack, file)));
						all.val = ::(path, all.val);
						Hashtbl.add(hxpack_to_jpack, path, (new Tuple(pack, file)));
					} else {
						if ( && ( = (Unix.stat(filepath).st_kind, S_DIR), && (<>(file, "."), <>(file, "..")))) {
							var pack = @(pack, ::(file, []));
							iter_files(pack, Unix.opendir(filepath), filepath);
						} else {
							[];
						};
					};
					iter_files(pack, dir, path);
				} catch (e: End_of_file | Unix.Unix_error(_)) {
					Unix.closedir(dir);
				};
			};
			iter_files([], Unix.opendir(file), file);
			var all = all.val;
			(new Tuple(function (pack, name): var real_path = ^ (file, ^ ("/", ^ (String.concat("/", pack), ^ ("/", ^ (name,
					   ".class")))));
			try {
				var data = Std.input_file(bin = True, real_path);
				Some(JReader.parse_class(IO.input_string(data)), real_path, real_path);
			} catch (e: _) {
				None;
			}, function []: [], function []: all));
		case _: var closed = ref(False);
			var zip = ref(Zip.open_in(file));
			function check_open([]) return {
				if (closed.val) {
					prerr_endline( ^ ("JAR file ", ^ (file, " already closed")));
					zip.val = Zip.open_in(file);
					closed.val = False;
				} else {
					[];
				};
			};
		List.iter(function case {
				Zip.is_directory = False;
				Zip.filename = filename
			} if (String.ends_with(filename, ".class")): var pack = String.nsplit(filename, "/");
			switch (List.rev(pack)) {
			case []: [];
				case ::(name, pack): var name = String.sub(name, 0, -(String.length(name), 6));
					var pack = List.rev(pack);
					Hashtbl.add(hxpack_to_jpack, jpath_to_hx((new Tuple(pack, name))), (new Tuple(pack, name)));
				};
		case _: [], Zip.entries(zip.val));
			(new Tuple(function (pack, name): check_open([]);
			try {
				var location = ^ (String.concat("/", @(pack, ::(name, []))), ".class");
				var entry = Zip.find_entry(zip.val, location);
				var data = Zip.read_entry(zip.val, entry);
				Some(JReader.parse_class(IO.input_string(data)), file, ^ (file, ^ ("@", location)));
			} catch (e: Not_found) {
				None;
			}, function []:
			if (!(closed.val)) {
			closed.val = True;
			Zip.close_in(zip.val);
			} else {
				[];
			}, function []: check_open([]);
			get_classes_zip(zip.val)));
		};
		var cached_types = Hashtbl.create(12);
		function get_raw_class(path) return {
			try {
				Hashtbl.find(cached_types, path);
			} catch (e: Not_found) {
				try {
					var Tuple(pack, name) = Hashtbl.find(hxpack_to_jpack, path);
					function try_file(Tuple(pack, name)) return {
						switch (get_raw_class((new Tuple(pack, name)))) {
						case None: Hashtbl.add(cached_types, path, None);
							None;
						case Some(i, p1, p2): Hashtbl.add(cached_types, path, Some(i, p1, p2));
							var ret = Some(normalize_jclass(com, i), p1, p2);
							Hashtbl.replace(cached_types, path, ret);
							ret;
						};
					};
					try_file((new Tuple(pack, name)));
				} catch (e: Not_found) {
					None;
				};
			};
		};
		function replace_canonical_name(p, pack, name_original, name_replace, decl) return {
			function mk_meta(name) return {
				(new Tuple(Meta.JavaCanonical, ::((new Tuple(EConst(String(String.concat(".", pack))), p)), ::((new Tuple(EConst(String(name)), p)), [])), p));
			};
			function add_meta(name, metas) return {
				if (Meta.has(Meta.JavaCanonical, metas)) {
				List.map(function case (Meta.JavaCanonical, ::((EConst(String(cpack)), _), ::((EConst(String(cname)), _), [])), _): var
						Tuple(did_replace, name) = String.replace(cname, name_original, name_replace);
						if (!(did_replace)) {
						print_endline( ^ (cname, ^ (" -> ", ^ (name_original, ^ (" -> ", name_replace)))));
						} else {
							[];
						};
					mk_meta(name);
				case m: m, metas);
				} else {
					::(mk_meta(name), metas);
				};
			};
			switch (decl) {
			case EClass(c): EClass({ (c) with d_meta = add_meta(c.d_name, c.d_meta) });
			case EEnum(e): EEnum({ (e) with d_meta = add_meta(e.d_name, e.d_meta) });
			case EAbstract(a): EAbstract({ (a) with d_meta = add_meta(a.d_name, a.d_meta) });
			case d: d;
			};
		};
		function build(ctx, path, p, types) return {
			try {
				if (List.mem(path, types.val)) {
					None;
				} else {
					var first = switch (types.val) {
					case ::((::(java, ::(lang, [])), String), []) | []: True;
					case ::(p, _): False;
					};
					types.val = ::(path, types.val);
					switch ((new Tuple(get_raw_class(path), path))) {
					case (None, ([], c)): build(ctx, (new Tuple(::("haxe", ::("root", [])), c)), p, types);
					case (None, _): None;
					case (Some(cls, real_path, pos_path), _): var is_disallowed_inner = && (first, String.exists(snd(cls.cpath), "$"));
						var is_disallowed_inner = if (is_disallowed_inner) {
							var Tuple(outer, inner) = String.split(snd(cls.cpath), "$");
							switch (get_raw_class((new Tuple(fst(path), outer)))) {
							case None: False;
							case _: True;
							};
						} else {
							False;
						};
						if (is_disallowed_inner) {
							None;
						} else {
							if (com.verbose) {
								print_endline( ^ ("Parsed Java class ", path_s(cls.cpath)));
							} else {
								[];
							};
							var old_types = ctx.jtparams;
							ctx.jtparams = ::(cls.ctypes, ctx.jtparams);
							var pos = { () with pfile = pos_path;
										pmin = 0;
										pmax = 0
									  };
							var pack = switch (fst(path)) {
							case ::(haxe, ::(root, [])): [];
							case p: p;
							};
							var ppath = Hashtbl.find(hxpack_to_jpack, path);
							var inner = List.fold_left(function acc: function (path, out, _, _): var path = jpath_to_hx(path);
							if (<>(out, Some(ppath))) {
							acc;
						} else {
							switch (build(ctx, path, p, types)) {
								case Some(_, (_, classes)): var base = ^ (snd(ppath), "$");
									@(List.map(function (def, p): (new Tuple(replace_canonical_name(p, fst(ppath), base, ^ (snd(ppath), "."), def), p)),
											   classes), acc);
								case _: acc;
								};
							}, [], cls.cinner_types);
							var inner = try {
								if (!(List.mem(JInterface, cls.cflags))) {
									raise(Not_found);
								} else {
									[];
								};
								var smethods = List.filter(function f: List.mem(JStatic, f.jf_flags), cls.cmethods);
								var sfields = List.filter(function f: List.mem(JStatic, f.jf_flags), cls.cfields);
								if (!( || (<>(smethods, []), <>(sfields, [])))) {
									raise(Not_found);
								} else {
									[];
								};
								var obj = TObject((new Tuple(::("java", ::("lang", [])), "Object")), []);
								var ncls = convert_java_class(ctx, pos, { (cls) with cmethods = smethods;
															  cfields = sfields;
															  cflags = [];
															  csuper = obj;
															  cinterfaces = [];
															  cinner_types = [];
															  ctypes = []
																		});
								switch (ncls) {
								case ::(EClass(c), imports): @(::((new Tuple(EClass({ (c) with d_name = ^ (c.d_name, "_Statics") }), pos)), inner),
																   List.map(function i: (new Tuple(i, pos)), imports));
								case _: assert False;
								};
							} catch (e: Not_found) {
								inner;
							};
							var inner_alias = ref(SS.empty);
							List.iter(function x:
							switch (fst(x)) {
						case EClass(c): inner_alias.val = SS.add(c.d_name, inner_alias.val);
							case _: [];
							}, inner);
							var alias_list = ref([]);
							List.iter(function x:
							switch (x) {
						case (EClass(c), pos): var parts = String.nsplit(c.d_name, "_24");
								switch (parts) {
								case ::(_, _): var alias_name = String.concat("_", parts);
									if ( && (!(SS.mem(alias_name, inner_alias.val)), !(String.exists(snd(path), "_24")))) {
										var alias_def = ETypedef({ () with d_name = alias_name;
																   d_doc = None;
																   d_params = c.d_params;
																   d_meta = [];
																   d_flags = [];
																   d_data = CTPath({ () with tpackage = pack;
																			tname = snd(path);
																			tparams = List.map(function tp: TPType(CTPath({ () with tpackage = [];
																					tname = tp.tp_name;
																					tparams = [];
																					tsub = None
																														  })), c.d_params);
																			tsub = Some(c.d_name)
																				   })
																 });
										inner_alias.val = SS.add(alias_name, inner_alias.val);
										alias_list.val = ::((new Tuple(alias_def, pos)), alias_list.val);
									} else {
										[];
									};
								case _: [];
								};
							case _: [];
							}, inner);
							var inner = List.concat(::(alias_list.val, ::(inner, [])));
							var classes = List.map(function t: (new Tuple(t, pos)), convert_java_class(ctx, pos, cls));
						var Tuple(imports, defs) = List.partition(function case (EImport(_), _): True;
													   case _: False, @(classes, inner));
							var ret = Some(real_path, (new Tuple(pack, @(imports, defs))));
							ctx.jtparams = old_types;
							ret;
						};
					};
				};
			} catch (e: T) {
				McOr(McArr(PaApp(PaId(IdAcc(<...>, <...>)), PaId(IdLid(<...>))), ExNil, ExSeq(ExSem(ExApp(<...>, <...>), ExId(<...>)))),
					 McArr(PaId(IdLid(e)), ExNil, ExSeq(ExSem(ExIfe(<...>, <...>, <...>),
													ExId(<...>)))))				case JReader.Error_message(msg): prerr_endline( ^ ("Class reader failed: ", msg));
				None;
			case e: if (com.verbose) {
					prerr_endline(Printexc.to_string(e));
				} else {
					[];
				};
				None;
			};
		};
		function build(path, p) return {
			build(create_ctx(com), path, p, ref(::((new Tuple(::("java", ::("lang", [])), "String")), [])));
		};
		var cached_files = ref(None);
		function list_all_files([]) return {
			switch (cached_files.val) {
			case None: var ret = list_all_files([]);
				cached_files.val = Some(ret);
				ret;
			case Some(r): r;
			};
		};
		com.load_extern_type = @(com.load_extern_type, ::(build, []));
		com.java_libs = ::((new Tuple(file, std, close, list_all_files, get_raw_class)), com.java_libs);
	}
}
;
