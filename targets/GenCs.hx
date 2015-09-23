import Gencommon.ReflectionCFs;
import Ast;
import Common;
import Type;
import Gencommon;
import Gencommon.SourceWriter;
import Printf;
import Option;
import ExtString;
import IlData;
import IlMeta;

typedef Net_lib_ctx = {
	nstd : Bool,
	ncom : Common.Context,
	nil : IlData.Ilctx
};

enum Il_any_field {
	IlField(value: Ilfield);
	IlMethod(value: Ilmethod);
	IlProp(value: Ilprop);
};

class Gencs {
	public static function netname_to_hx(name) return {
		var len = String.length(name);
		var chr = String.get(name, 0);
		^ (String.make(1, Char.uppercase(chr)), String.sub(name, 1, -(len, 1)));
	};

	public static function is_cs_basic_type(t) return {
		switch (follow(t)) {
		case TInst({ cl_path = (::(haxe, []), Int32) }, []) | TInst({ cl_path = (::(haxe, []), Int64) }, []) | TAbstract({ a_path = (::(cs, []), Int64) }, []) | TAbstract({ a_path = (::(cs, []), UInt64) }, []) | TAbstract({ a_path = ([], Int) }, []) | TAbstract({ a_path = ([], Float) }, []) | TAbstract({ a_path = ([], Bool) }, [])
			: True;
		case TAbstract({ a_path = (::(cs, []), Pointer) }, _): False;
		case TAbstract(_) if (like_float(t)): True;
		case TAbstract(a, pl) if (!(Meta.has(Meta.CoreType, a.a_meta))): is_cs_basic_type(Abstract.get_underlying_type(a, pl));
		case TEnum(e, _) if (!(Meta.has(Meta.Class, e.e_meta))): True;
		case TInst(cl, _) if (Meta.has(Meta.Struct, cl.cl_meta)): True;
		case _: False;
		};
	};

	public static var cs_binops = ::((new Tuple(Ast.OpAdd, "op_Addition")), ::((new Tuple(Ast.OpSub, "op_Subtraction")),
									 ::((new Tuple(Ast.OpMult, "op_Multiply")), ::((new Tuple(Ast.OpDiv, "op_Division")), ::((new Tuple(Ast.OpMod,
											 "op_Modulus")), ::((new Tuple(Ast.OpXor, "op_ExclusiveOr")), ::((new Tuple(Ast.OpOr, "op_BitwiseOr")),
													 ::((new Tuple(Ast.OpAnd, "op_BitwiseAnd")), ::((new Tuple(Ast.OpBoolAnd, "op_LogicalAnd")), ::((new Tuple(Ast.OpBoolOr,
															 "op_LogicalOr")), ::((new Tuple(Ast.OpAssign, "op_Assign")), ::((new Tuple(Ast.OpShl, "op_LeftShift")),
																	 ::((new Tuple(Ast.OpShr, "op_RightShift")), ::((new Tuple(Ast.OpShr, "op_SignedRightShift")), ::((new Tuple(Ast.OpUShr,
																			 "op_UnsignedRightShift")), ::((new Tuple(Ast.OpEq, "op_Equality")), ::((new Tuple(Ast.OpGt, "op_GreaterThan")),
																					 ::((new Tuple(Ast.OpLt, "op_LessThan")), ::((new Tuple(Ast.OpNotEq, "op_Inequality")), ::((new Tuple(Ast.OpGte,
																							 "op_GreaterThanOrEqual")), ::((new Tuple(Ast.OpLte, "op_LessThanOrEqual")), ::((new Tuple(Ast.OpAssignOp(Ast.OpMult),
																									 "op_MultiplicationAssignment")), ::((new Tuple(Ast.OpAssignOp(Ast.OpSub), "op_SubtractionAssignment")),
																											 ::((new Tuple(Ast.OpAssignOp(Ast.OpXor), "op_ExclusiveOrAssignment")), ::((new Tuple(Ast.OpAssignOp(Ast.OpShl),
																													 "op_LeftShiftAssignment")), ::((new Tuple(Ast.OpAssignOp(Ast.OpMod), "op_ModulusAssignment")),
																															 ::((new Tuple(Ast.OpAssignOp(Ast.OpAdd), "op_AdditionAssignment")), ::((new Tuple(Ast.OpAssignOp(Ast.OpAnd),
																																	 "op_BitwiseAndAssignment")), ::((new Tuple(Ast.OpAssignOp(Ast.OpOr), "op_BitwiseOrAssignment")),
																																			 ::((new Tuple(Ast.OpAssignOp(Ast.OpDiv), "op_DivisionAssignment")), []))))))))))))))))))))))))))))));

	public static var cs_unops = ::((new Tuple(Ast.Decrement, "op_Decrement")), ::((new Tuple(Ast.Increment, "op_Increment")),
									::((new Tuple(Ast.Neg, "op_UnaryNegation")), ::((new Tuple(Ast.Not, "op_LogicalNot")), ::((new Tuple(Ast.NegBits,
											"op_OnesComplement")), [])))));

	public static var binops_names = List.fold_left(function acc: function (op, n): PMap.add(n, op, acc), PMap.empty,
									 cs_binops);

	public static var unops_names = List.fold_left(function acc: function (op, n): PMap.add(n, op, acc), PMap.empty, cs_unops);

	public static var get_item = "get_Item";

	public static var set_item = "set_Item";

	public static function is_tparam(t) return {
		switch (follow(t)) {
		case TInst({ cl_kind = KTypeParameter(_) }, []): True;
		case _: False;
		};
	};

	public static function is_int_float(gen, t) return {
		switch (follow(gen.greal_type(t))) {
		case TInst({ cl_path = (::(haxe, []), Int32) }, []) | TAbstract({ a_path = ([], Int) }, []) | TAbstract({ a_path = ([], Float) }, [])
			: True;
		case TAbstract(_) if (&&(like_float(t), !(like_i64(t)))): True;
		case TInst({ cl_path = (::(haxe, ::(lang, [])), Null) }, ::(t, [])): is_int_float(gen, t);
		case _: False;
		};
	};

	public static function is_bool(t) return {
		switch (follow(t)) {
		case TAbstract({ a_path = ([], Bool) }, []): True;
		case _: False;
		};
	};

	public static function is_exactly_bool(gen, t) return {
		switch (gen.gfollowrun_f(t)) {
		case TAbstract({ a_path = ([], Bool) }, []): True;
		case _: False;
		};
	};

	public static function is_dynamic(gen, t) return {
		switch (follow(gen.greal_type(t))) {
		case TDynamic(_): True;
		case _: False;
		};
	};

	public static function is_pointer(gen, t) return {
		switch (follow(gen.greal_type(t))) {
		case TAbstract({ a_path = (::(cs, []), Pointer) }, _) | TInst({ cl_path = (::(cs, []), Pointer) }, _): True;
		case _: False;
		};
	};

	public static function is_null(t) return {
		switch (t) {
		case TInst({ cl_path = (::(haxe, ::(lang, [])), Null) }, _) | TType({ t_path = ([], Null) }, _): True;
		case TType(t, tl): is_null(apply_params(t.t_params, tl, t.t_type));
		case TMono(r): switch (r.val) {
			case Some(t): is_null(t);
			case _: False;
			};
		case TLazy(f): is_null(f.val([]));
		case _: False;
		};
	};

	public static function get_ptr(e) return {
		switch (e.eexpr) {
		case TParenthesis(e) | TMeta(_, e) | TCast(e, _): get_ptr(e);
		case TCall({ eexpr = TLocal({ v_name = __ptr__ }) }, ::(e, [])): Some(e);
		case _: None;
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

	public static function change_md(match) return switch (match) {
	case TAbstractDecl(a) if (&&(Meta.has(Meta.Delegate, a.a_meta), !(Meta.has(Meta.CoreType, a.a_meta)))): change_md(t_to_md(
					a.a_this));
	case TClassDecl({ cl_kind = KAbstractImpl({ a_this = TInst(impl, _) } = a) }) if (Meta.has(Meta.Delegate, a.a_meta)):
		TClassDecl(impl);
	case md: md;
	};

	public static function add_cast_handler(gen) return {
		var basic = gen.gcon.basic;
		var native_arr_cl = get_cl(get_type(gen, (new Tuple(::("cs", []), "NativeArray"))));
		function get_narr_param(t) return {
			switch (follow(t)) {
			case TInst({ cl_path = (::(cs, []), NativeArray) }, ::(param, [])): param;
			case _: assert False;
			};
		};
		function gtparam_cast_native_array(e, to_t) return {
			var old_param = get_narr_param(e.etype);
			var new_param = get_narr_param(to_t);
			var new_v = mk_temp(gen, "new_arr", to_t);
			var i = mk_temp(gen, "i", basic.tint);
			var old_len = mk_field_access(gen, e, "Length", e.epos);
			var obj_v = mk_temp(gen, "obj", t_dynamic);
			var check_null = {
				() with eexpr = TBinop(Ast.OpNotEq, e, null(e.etype, e.epos));
				etype = basic.tbool;
				epos = e.epos
			};
			var block = ::({ () with eexpr = TVar(new_v, Some({ () with eexpr = TNew(native_arr_cl, ::(new_param, []), ::(old_len, []));
												  etype = to_t;
												  epos = e.epos
															  }));
							 etype = basic.tvoid;
							 epos = e.epos
						   }, ::({ () with eexpr = TVar(i, Some(mk_int(gen, -1, e.epos)));
								   etype = basic.tvoid;
								   epos = e.epos
								 }, ::({ () with eexpr = TWhile({ () with eexpr = TBinop(Ast.OpLt, { () with eexpr = TUnop(Ast.Increment, Ast.Prefix, mk_local(i, e.epos));
										 etype = basic.tint;
										 epos = e.epos
																								   }, old_len);
										 etype = basic.tbool;
										 epos = e.epos
																}, { () with eexpr = TBlock(::({ () with eexpr = TVar(obj_v, Some(mk_cast(t_dynamic, { () with eexpr = TArray(e, mk_local(i, e.epos));
																		etype = old_param;
																		epos = e.epos
																																					 })));
																		etype = basic.tvoid;
																		epos = e.epos
																							   }, ::({ () with eexpr = TIf({ () with eexpr = TBinop(Ast.OpNotEq, mk_local(obj_v, e.epos), null(e.etype, e.epos));
																									   etype = basic.tbool;
																									   epos = e.epos
																														   }, { () with eexpr = TBinop(Ast.OpAssign, { () with eexpr = TArray(mk_local(new_v, e.epos), mk_local(i, e.epos));
																																   etype = new_param;
																																   epos = e.epos
																																									 }, mk_cast(new_param, mk_local(obj_v, e.epos)));
																																   etype = new_param;
																																   epos = e.epos
																															  }, None);
																									   etype = basic.tvoid;
																									   epos = e.epos
																									 }, [])));
																		etype = basic.tvoid;
																		epos = e.epos
																   }, Ast.NormalWhile);
										 etype = basic.tvoid;
										 epos = e.epos
									   }, ::(mk_local(new_v, e.epos), []))));
			{
				() with eexpr = TIf(check_null, {
					() with eexpr = TBlock(block);
					etype = to_t;
					epos = e.epos
				}, Some(null(new_v.v_type, e.epos)));
				etype = to_t;
				epos = e.epos
			};
		};
		Hashtbl.add(gen.gtparam_cast, (new Tuple(::("cs", []), "NativeArray")), gtparam_cast_native_array);
	};

	public static function handle_type_params(gen, ifaces, base_generic) return {
		add_cast_handler(gen);
		TypeParams.RealTypeParams.default_config(gen, function e: function t: gen.gcon.warning( ^ ("Cannot cast to ", debug_type(t)), e.epos);
		mk_cast(t, e), ifaces, base_generic);
	};

	public static var connecting_string = "?";

	public static var default_package = "cs";

	public static var strict_mode = ref(False);

	public static var reserved = var res = Hashtbl.create(120);
	List.iter(function lst: Hashtbl.add(res, lst, ^ ("@", lst)), ::("abstract", ::("as", ::("base", ::("bool", ::("break",
			  ::("byte", ::("case", ::("catch", ::("char", ::("checked", ::("class", ::("const", ::("continue", ::("decimal",
									   ::("default", ::("delegate", ::("do", ::("double", ::("else", ::("enum", ::("event", ::("explicit", ::("extern",
											   ::("false", ::("finally", ::("fixed", ::("float", ::("for", ::("foreach", ::("goto", ::("if", ::("implicit", ::("in",
													   ::("int", ::("interface", ::("internal", ::("is", ::("lock", ::("long", ::("namespace", ::("new", ::("null", ::("object",
															   ::("operator", ::("out", ::("override", ::("params", ::("private", ::("protected", ::("public", ::("readonly", ::("ref",
																	   ::("return", ::("sbyte", ::("sealed", ::("short", ::("sizeof", ::("stackalloc", ::("static", ::("string", ::("struct",
																			   ::("switch", ::("this", ::("throw", ::("true", ::("try", ::("typeof", ::("uint", ::("ulong", ::("unchecked", ::("unsafe",
																					   ::("ushort", ::("using", ::("virtual", ::("volatile", ::("void", ::("while", ::("add", ::("ascending", ::("by",
																							   ::("descending", ::("dynamic", ::("equals", ::("from", ::("get", ::("global", ::("group", ::("into", ::("join", ::("let",
																									   ::("on", ::("orderby", ::("partial", ::("remove", ::("select", ::("set", ::("value", ::("var", ::("where",
																											   ::("yield", [])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
	res;

	public static var dynamic_anon = TAnon({ () with a_fields = PMap.empty;
										   a_status = ref(Closed)
										   });

	public static function get_class_modifiers(meta, cl_type, cl_access, cl_modifiers) return {
		switch (meta) {
		case []: (new Tuple(cl_type, cl_access, cl_modifiers));
		case ::((Meta.Struct, [], _), meta): get_class_modifiers(meta, "struct", cl_access, cl_modifiers);
		case ::((Meta.Protected, [], _), meta): get_class_modifiers(meta, cl_type, "protected", cl_modifiers);
		case ::((Meta.Internal, [], _), meta): get_class_modifiers(meta, cl_type, "internal", cl_modifiers);
		case ::((Meta.Final, [], _), meta): get_class_modifiers(meta, cl_type, cl_access, ::("sealed", cl_modifiers));
		case ::((Meta.Unsafe, [], _), meta): get_class_modifiers(meta, cl_type, cl_access, ::("unsafe", cl_modifiers));
		case ::(_, meta): get_class_modifiers(meta, cl_type, cl_access, cl_modifiers);
		};
	};

	public static function get_fun_modifiers(meta, access, modifiers) return {
		switch (meta) {
		case []: (new Tuple(access, modifiers));
		case ::((Meta.Protected, [], _), meta): get_fun_modifiers(meta, "protected", modifiers);
		case ::((Meta.Internal, [], _), meta): get_fun_modifiers(meta, "internal", modifiers);
		case ::((Meta.ReadOnly, [], _), meta): get_fun_modifiers(meta, access, ::("readonly", modifiers));
		case ::((Meta.Unsafe, [], _), meta): get_fun_modifiers(meta, access, ::("unsafe", modifiers));
		case ::((Meta.Volatile, [], _), meta): get_fun_modifiers(meta, access, ::("volatile", modifiers));
		case ::((Meta.Custom(?prop_impl | ?event_impl), [], _), meta): get_fun_modifiers(meta, "private", modifiers);
		case ::(_, meta): get_fun_modifiers(meta, access, modifiers);
		};
	};

	public static function configure(gen) return {
		var native_arr_cl = get_cl(get_type(gen, (new Tuple(::("cs", []), "NativeArray"))));
		gen.gclasses.nativearray = function t: TInst(native_arr_cl, ::(t, []));
	gen.gclasses.nativearray_type = function case TInst(_, ::(t, [])): t;
	case _: assert False;
		gen.gclasses.nativearray_len = function e: function p: mk_field_access(gen, e, "Length", p);
		var basic = gen.gcon.basic;
		var erase_generics = Common.defined(gen.gcon, Define.EraseGenerics);
		var fn_cl = get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "Function"))));
		var null_t = if (erase_generics) {
			null_class;
		} else {
			get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "Null"))));
		};
		var runtime_cl = get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "Runtime"))));
		var no_root = Common.defined(gen.gcon, Define.NoRoot);
		function change_id(name) return {
			try {
				Hashtbl.find(reserved, name);
			} catch (e: Not_found) {
				var ret = String.concat(".", String.nsplit(name, "#"));
				List.hd(String.nsplit(ret, "`"));
			};
		};
		function change_clname(n) return {
			change_id(n);
		};
		function change_ns_params_root(md, ns, params) return {
			var Tuple(ns, params) = List.fold_left(function (ns, params): function nspart:
			try {
				var Tuple(part, nparams) = String.split(nspart, "`");
				var nparams = int_of_string(nparams);
				function loop(i, needed, params) return {
					if ( = (i, nparams)) {
						(new Tuple(List.rev(needed), params));
					} else {
						loop(+(i, 1), ::(List.hd(params), needed), List.tl(params));
					};
				};
				var Tuple(needed, params) = loop(0, [], params);
				var part = change_id(part);
				(new Tuple(::( ^ (part, ^ ("<", ^ (String.concat(", ", needed), ">"))), ns), params));
			} catch (e: _) {
				(new Tuple(::(change_id(nspart), ns), params));
			}, (new Tuple([], params)), ns);
			(new Tuple(List.rev(ns), params));
		};
		function change_ns_params(md, params, ns) return {
			if (no_root) {
				switch (ns) {
				case [] if (is_hxgen(md)): (new Tuple(::("haxe", ::("root", [])), params));
				case []: switch (md) {
					case TClassDecl({ cl_path = ([], Std) | ([], Math) }): (new Tuple(::("haxe", ::("root", [])), params));
					case TClassDecl({ cl_meta = m }) if (Meta.has(Meta.Enum, m)): (new Tuple(::("haxe", ::("root", [])), params));
					case _: (new Tuple([], params));
					};
				case ns if (=(params, [])): (new Tuple(List.map(change_id, ns), params));
				case ns: change_ns_params_root(md, ns, params);
				};
			} else {
				if ( = (params, [])) {
					(new Tuple(List.map(change_id, ns), params));
				} else {
					change_ns_params_root(md, ns, params);
				};
			};
		};
		function change_ns(md, ns) return {
			var Tuple(ns, _) = change_ns_params(md, [], ns);
			ns;
		};
		var change_field = change_id;
		function write_id(w, name) return {
			write(w, change_id(name));
		};
		function write_field(w, name) return {
			write(w, change_field(name));
		};
		var ptr = if (Common.defined(gen.gcon, Define.Unsafe)) {
			get_abstract(get_type(gen, (new Tuple(::("cs", []), "Pointer"))));
		} else {
			null_abstract;
		};
		function is_hxgeneric(md) return {
			TypeParams.RealTypeParams.is_hxgeneric(md);
		};
		function field_is_hxgeneric(e) return {
			switch (e.eexpr) {
			case TParenthesis(e) | TMeta(_, e): field_is_hxgeneric(e);
			case TField(_, FStatic(cl, _) | FInstance(cl, _, _)): is_hxgeneric(TClassDecl(cl));
			case _: True;
			};
		};
		gen.gfollowadd(name = "follow_basic", function t:
		switch (t) {
	case TAbstract({ a_path = ([], Bool) }, []) | TAbstract({ a_path = ([], Void) }, []) | TAbstract({ a_path = ([], Float) }, []) | TAbstract({ a_path = ([], Int) }, []) | TAbstract({ a_path = ([], UInt) }, []) | TType({ t_path = (::(cs, []), Int64) }, []) | TAbstract({ a_path = (::(cs, []), Int64) }, []) | TType({ t_path = (::(cs, []), UInt64) }, []) | TAbstract({ a_path = (::(cs, []), UInt64) }, []) | TType({ t_path = (::(cs, []), UInt8) }, []) | TAbstract({ a_path = (::(cs, []), UInt8) }, []) | TType({ t_path = (::(cs, []), Int8) }, []) | TAbstract({ a_path = (::(cs, []), Int8) }, []) | TType({ t_path = (::(cs, []), Int16) }, []) | TAbstract({ a_path = (::(cs, []), Int16) }, []) | TType({ t_path = (::(cs, []), UInt16) }, []) | TAbstract({ a_path = (::(cs, []), UInt16) }, []) | TType({ t_path = (::(cs, []), Char16) }, []) | TAbstract({ a_path = (::(cs, []), Char16) }, []) | TType({ t_path = (::(cs, []), Ref) }, _) | TAbstract({ a_path = (::(cs, []), Ref) }, _) | TType({ t_path = (::(cs, []), Out) }, _) | TAbstract({ a_path = (::(cs, []), Out) }, _) | TType({ t_path = ([], Single) }, []) | TAbstract({ a_path = ([], Single) }, [])
				: Some(t);
		case TType({ t_path = ([], Null) } = tdef, ::(t2, [])): Some(TType(tdef, ::(follow(gen.gfollowrun_f(t2)), [])));
		case TAbstract({ a_path = (::(cs, []), PointerAccess) }, ::(t, [])): Some(TAbstract(ptr, ::(t, [])));
		case TAbstract(a, pl) if (!(Meta.has(Meta.CoreType, a.a_meta))): Some(gen.gfollowrun_f(Abstract.get_underlying_type(a,
					pl)));
		case TAbstract({ a_path = ([], EnumValue) }, _) | TInst({ cl_path = ([], EnumValue) }, _): Some(t_dynamic);
		case _: None;
		});
		function module_s_params(md, params) return {
			var md = change_md(md);
			var path = t_infos(md).mt_path;
			switch (path) {
			case ([], String): (new Tuple("string", params));
			case ([], Null): (new Tuple(path_s((new Tuple(change_ns(md, ::("haxe", ::("lang", []))), change_clname("Null")))),
				params));
			case (ns, clname): var Tuple(ns, params) = change_ns_params(md, params, ns);
				(new Tuple(path_s((new Tuple(ns, change_clname(clname)))), params));
			};
		};
		function module_s(md) return {
			fst(module_s_params(md, []));
		};
		var ifaces = Hashtbl.create(1);
		var ti64 = switch (get_type(gen, (new Tuple(::("cs", []), "Int64")))) {
		case TTypeDecl(t): TType(t, []);
		case TAbstractDecl(a): TAbstract(a, []);
		case _: assert False;
		};
		var ttype = get_cl(get_type(gen, (new Tuple(::("System", []), "Type"))));
		function has_tdyn(tl) return {
			List.exists(function t:
			switch (follow(t)) {
		case TDynamic(_) | TMono(_): True;
			case _: False;
			}, tl);
		};
		function real_type(t) return {
			var t = gen.gfollowrun_f(t);
			var ret = switch (t) {
			case TAbstract(a, pl) if (!(Meta.has(Meta.CoreType, a.a_meta))): real_type(Abstract.get_underlying_type(a, pl));
			case TAbstract({ a_path = (::(cs, ::(_Flags, [])), EnumUnderlying) }, ::(t, [])): real_type(t);
			case TInst({ cl_path = (::(cs, ::(system, [])), String) }, []): gen.gcon.basic.tstring;
			case TInst({ cl_path = (::(haxe, []), Int32) }, []): gen.gcon.basic.tint;
			case TInst({ cl_path = (::(haxe, []), Int64) }, []): ti64;
			case TAbstract({ a_path = ([], Class) }, _) | TAbstract({ a_path = ([], Enum) }, _) | TAbstract({ a_path = (::(haxe, ::(extern, [])), Rest) }, _) | TInst({ cl_path = ([], Class) }, _) | TInst({ cl_path = ([], Enum) }, _)
					: TInst(ttype, []);
			case TInst({ cl_kind = KTypeParameter(_) } = cl, _) if (&&(erase_generics, !(Meta.has(Meta.NativeGeneric, cl.cl_meta)))):
				t_dynamic;
			case TInst({ cl_kind = KExpr(_) }, _): t_dynamic;
			case TEnum(_, []) | TInst(_, []): t;
			case TInst(cl, params) if (&&(has_tdyn(params), Hashtbl.mem(ifaces, cl.cl_path))): TInst(Hashtbl.find(ifaces,
						cl.cl_path), []);
			case TEnum(e, params): TEnum(e, List.map(function _: t_dynamic, params));
			case TInst(cl, params) if (Meta.has(Meta.Enum, cl.cl_meta)): TInst(cl, List.map(function _: t_dynamic, params));
			case TInst(cl, params): TInst(cl, change_param_type(TClassDecl(cl), params));
			case TType({ t_path = ([], Null) }, ::(t, [])):
				if (erase_generics) {
					if (is_cs_basic_type(t)) {
						t_dynamic;
					} else {
						real_type(t);
					};
				} else {
					switch (real_type(t)) {
					case TInst({ cl_kind = KTypeParameter(_) }, _): TInst(null_t, ::(t, []));
					case _ if (is_cs_basic_type(t)): TInst(null_t, ::(t, []));
					case _: real_type(t);
					};
				};
			case TAbstract(_) | TType(_): t;
			case TAnon(anon) if (switch (anon.a_status.val) {
					case Statics(_) | EnumStatics(_) | AbstractStatics(_): True;
						case _: False;
						}): t;
			case TFun(_): TInst(fn_cl, []);
			case _: t_dynamic;
			};
			ret;
		};
		function change_param_type(md, tl) return {
			var types = switch (md) {
			case TClassDecl(c): c.cl_params;
			case TEnumDecl(e): [];
			case TAbstractDecl(a): a.a_params;
			case TTypeDecl(t): t.t_params;
			};
			var is_hxgeneric = if ( = (types, [])) {
				is_hxgen(md);
			} else {
				TypeParams.RealTypeParams.is_hxgeneric(md);
			};
			function ret(t) return {
				var t_changed = real_type(t);
				switch ((new Tuple(is_hxgeneric, t_changed))) {
				case (False, _): t;
				case (True, TInst({ cl_path = (::(haxe, ::(lang, [])), Null) }, _)): dynamic_anon;
				case (True, TInst({ cl_kind = KTypeParameter(_) }, _)): t;
				case (True, TInst(_)) | (True, TEnum(_)) | (True, TAbstract(_)) if (is_cs_basic_type(t_changed)): t;
				case (True, TDynamic(_)): t;
				case (True, x): dynamic_anon;
				};
			};
			if ( && (is_hxgeneric, || (erase_generics, List.exists(function t:
			switch (follow(t)) {
			case TDynamic(_): True;
				case _: False;
				}, tl)))) {
				List.map(function _: t_dynamic, tl);
			} else {
				List.map(ret, tl);
			};
		};
		function is_dynamic(t) return {
			switch (real_type(t)) {
			case TMono(_) | TDynamic(_) | TInst({ cl_kind = KTypeParameter(_) }, _): True;
			case TAnon(anon): switch (anon.a_status.val) {
				case EnumStatics(_) | Statics(_): False;
				case _: True;
				};
			case _: False;
			};
		};
		function t_s(t) return {
			switch (real_type(t)) {
			case TAbstract({ a_path = ([], Bool) }, []): "bool";
			case TAbstract({ a_path = ([], Void) }, []): "object";
			case TAbstract({ a_path = ([], Float) }, []): "double";
			case TAbstract({ a_path = ([], Int) }, []): "int";
			case TAbstract({ a_path = ([], UInt) }, []): "uint";
			case TType({ t_path = (::(cs, []), Int64) }, []) | TAbstract({ a_path = (::(cs, []), Int64) }, []): "long";
			case TType({ t_path = (::(cs, []), UInt64) }, []) | TAbstract({ a_path = (::(cs, []), UInt64) }, []): "ulong";
			case TType({ t_path = (::(cs, []), UInt8) }, []) | TAbstract({ a_path = (::(cs, []), UInt8) }, []): "byte";
			case TType({ t_path = (::(cs, []), Int8) }, []) | TAbstract({ a_path = (::(cs, []), Int8) }, []): "sbyte";
			case TType({ t_path = (::(cs, []), Int16) }, []) | TAbstract({ a_path = (::(cs, []), Int16) }, []): "short";
			case TType({ t_path = (::(cs, []), UInt16) }, []) | TAbstract({ a_path = (::(cs, []), UInt16) }, []): "ushort";
			case TType({ t_path = (::(cs, []), Char16) }, []) | TAbstract({ a_path = (::(cs, []), Char16) }, []): "char";
			case TType({ t_path = ([], Single) }, []) | TAbstract({ a_path = ([], Single) }, []): "float";
			case TInst({ cl_path = (::(haxe, []), Int32) }, []) | TAbstract({ a_path = (::(haxe, []), Int32) }, []): "int";
			case TInst({ cl_path = (::(haxe, []), Int64) }, []) | TAbstract({ a_path = (::(haxe, []), Int64) }, []): "long";
			case TInst({ cl_path = ([], Dynamic) }, _) | TAbstract({ a_path = ([], Dynamic) }, _): "object";
			case TType({ t_path = (::(cs, []), Out) }, ::(t, [])) | TAbstract({ a_path = (::(cs, []), Out) }, ::(t, [])) | TType({ t_path = (::(cs, []), Ref) }, ::(t, [])) | TAbstract({ a_path = (::(cs, []), Ref) }, ::(t, []))
					: t_s(t);
			case TInst({ cl_path = (::(cs, []), NativeArray) }, ::(param, [])): 	function check_t_s(t) return {
					switch (real_type(t)) {
					case TInst({ cl_path = (::(cs, []), NativeArray) }, ::(param, [])): ^ (check_t_s(param), "[]");
					case _: t_s(run_follow(gen, t));
					};
				};
				^ (check_t_s(param), "[]");
			case TInst({ cl_path = (::(cs, []), Pointer) }, ::(t, [])) | TAbstract({ a_path = (::(cs, []), Pointer) }, ::(t, [])): var
				ret = t_s(t);
				^ (if ( = (ret, "object")) {
				"void";
			} else {
				ret;
			}, "*");
			case TInst({ cl_kind = KTypeParameter(_); cl_path = p }, []): snd(p);
			case TMono(r): switch (r.val) {
				case None: "object";
				case Some(t): t_s(run_follow(gen, t));
				};
			case TInst({ cl_path = ([], String) }, []): "string";
			case TEnum(e, params): ^ ("global::", module_s(TEnumDecl(e)));
			case TInst(cl, ::(_, _)) if (Meta.has(Meta.Enum, cl.cl_meta)): ^ ("global::", module_s(TClassDecl(cl)));
			case TInst({ cl_path = p } = cl, params): path_param_s(TClassDecl(cl), p, params);
			case TType({ t_path = p } = t, params): path_param_s(TTypeDecl(t), p, params);
			case TAnon(anon): switch (anon.a_status.val) {
				case Statics(_) | EnumStatics(_): "System.Type";
				case _: "object";
				};
			case TDynamic(_): "object";
			case TAbstract(a, pl) if (!(Meta.has(Meta.CoreType, a.a_meta))): t_s(Abstract.get_underlying_type(a, pl));
			case _: if (strict_mode.val) {
					trace( ^ ("[ !TypeError ", ^ (Type.s_type(Type.print_context([]), t), " ]")));
					assert False;
				} else {
					^ ("[ !TypeError ", ^ (Type.s_type(Type.print_context([]), t), " ]"));
				};
			};
		};
		function path_param_s(md, path, params) return {
			switch (params) {
			case []: ^ ("global::", module_s(md));
			case _ if (&&(erase_generics, is_hxgeneric(md))): ^ ("global::", module_s(md));
			case _: var params = List.map(function t: t_s(t), change_param_type(md, params));
				var Tuple(str, params) = module_s_params(md, params);
				if ( = (params, [])) {
					^ ("global::", str);
				} else {
					sprintf("global::%s<%s>", str, String.concat(", ", params));
				};
			};
		};
		function rett_s(t) return {
			switch (t) {
			case TAbstract({ a_path = ([], Void) }, []): "void";
			case _: t_s(t);
			};
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
			case c if (>(c, 0xFFFF)): Buffer.add_string(b, Printf.sprintf("\\U%.8x", c));
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
			case TBlock(_) | TFor(_) | TSwitch(_) | TTry(_) | TIf(_): False;
			case TWhile(_, _, flag) if (=(flag, Ast.NormalWhile)): False;
			case _: True;
			};
		};
		var in_value = ref(False);
		function md_s(md) return {
			var md = follow_module(gen.gfollowrun_f, md);
			switch (md) {
			case TClassDecl({ cl_params = [] } = cl): t_s(TInst(cl, []));
			case TClassDecl(cl) if (!(is_hxgen(md))): t_s(TInst(cl, List.map(function t: t_dynamic, cl.cl_params)));
			case TEnumDecl({ e_params = [] } = e): t_s(TEnum(e, []));
			case TEnumDecl(e) if (!(is_hxgen(md))): t_s(TEnum(e, List.map(function t: t_dynamic, e.e_params)));
			case TClassDecl(cl): t_s(TInst(cl, []));
			case TEnumDecl(e): t_s(TEnum(e, []));
			case TTypeDecl(t): t_s(TType(t, List.map(function t: t_dynamic, t.t_params)));
			case TAbstractDecl(a): t_s(TAbstract(a, List.map(function t: t_dynamic, a.a_params)));
			};
		};
		function ensure_local(e, explain) return {
			switch (e.eexpr) {
			case TLocal(_): e;
			case TCast(e, _) | TParenthesis(e) | TMeta(_, e): ensure_local(e, explain);
			case _: gen.gcon.error( ^ ("This function argument ", ^ (explain, " must be a local variable.")), e.epos);
				e;
			};
		};
		function ensure_refout(e, explain) return {
			switch (e.eexpr) {
			case TField(_) | TLocal(_): e;
			case TCast(e, _) | TParenthesis(e) | TMeta(_, e): ensure_refout(e, explain);
			case _: gen.gcon.error( ^ ("This function argument ", ^ (explain, " must be a local variable.")), e.epos);
				e;
			};
		};
		var last_line = ref(-1);
		function begin_block(w) return {
			write(w, "{");
			push_indent(w);
			newline(w);
			last_line.val = -1;
		};
		function end_block(w) return {
			pop_indent(w);
			if (w.sw_has_content) {
				newline(w);
			} else {
				[];
			};
			write(w, "}");
			newline(w);
			last_line.val = -1;
		};
		var skip_line_directives = || ( && (!(gen.gcon.debug), !(Common.defined(gen.gcon, Define.NoCompilation))), Common.defined(gen.gcon, Define.RealPosition));
		var line_directive = if (skip_line_directives) {
			function w: function p: [];
		} else {
			function w: function p:
			if (<>(p.pfile, Ast.null_pos.pfile)) {
				var cur_line = Lexer.get_error_line(p);
				var file = Common.get_full_path(p.pfile);
				if (<>(cur_line, +(last_line.val, 1))) {
					var line = Ast.s_escape(file);
					if ( <= (String.length(line), 256)) {
						print(w, "#line %d \"%s\"", cur_line, line);
						newline(w);
					} else {
						print(w, "//line %d \"%s\"", cur_line, line);
						newline(w);
					};
					last_line.val = cur_line;
				} else {
					[];
				};
			} else {
				[];
			};
		};
		var line_reset_directive = if (skip_line_directives) {
			function w: [];
		} else {
			function w: print(w, "#line default");
		};
		function extract_tparams(params, el) return {
			switch (el) {
			case ::( {
					eexpr = TLocal({ v_name = $type_param })
				} = tp, tl): extract_tparams(::(tp.etype, params), tl);
			case _: (new Tuple(params, el));
			};
		};
		function is_extern_prop(t, name) return {
			switch ((new Tuple(follow(run_follow(gen, t)), field_access(gen, t, name)))) {
			case (TInst({ cl_interface = True; cl_extern = True } = cl, _), FNotFound): !(is_hxgen(TClassDecl(cl)));
			case (_, FClassField(_, _, decl, v, _, t, _)): && (Type.is_extern_field(v), || (Meta.has(Meta.Property, v.cf_meta),
						&& (decl.cl_extern, !(is_hxgen(TClassDecl(decl))))));
			case _: False;
			};
		};
		function is_event(t, name) return {
			switch ((new Tuple(follow(run_follow(gen, t)), field_access(gen, t, name)))) {
			case (TInst({ cl_interface = True; cl_extern = True } = cl, _), FNotFound): !(is_hxgen(TClassDecl(cl)));
			case (_, FClassField(_, _, decl, v, _, _, _)): Meta.has(Meta.Event, v.cf_meta);
			case _: False;
			};
		};
		function extract_statements(expr) return {
			var ret = ref([]);
			function loop(expr) return {
				switch (expr.eexpr) {
				case TCall({ eexpr = TLocal({ v_name = __is__ | __typeof__ | __array__ | __sizeof__ | __delegate__ }) }, el): List.iter(
						loop, el);
				case TNew({ cl_path = (::(cs, []), NativeArray) }, params, ::(size, [])): [];
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
			last_line.val = -1;
			in_value.val = False;
			function expr_s(w, e) return {
				var was_in_value = in_value.val;
				in_value.val = True;
				switch (e.eexpr) {
				case TCall({ eexpr = TField(ef, f) }, ::(_, _) = args) if (=(field_name(f), "get_Item")): expr_s(w, ef);
					write(w, "[");
					var first = ref(True);
					List.iter(function f:
					if (first.val) {
					first.val = False;
				} else {
					write(w, ", ");
					};
					expr_s(w, f), args);
					write(w, "]");
				case TCall({ eexpr = TField(ef, f) }, ::(_, ::(_, _)) = args) if (=(field_name(f), "set_Item")): expr_s(w, ef);
					write(w, "[");
					var Tuple(args, value) = switch (List.rev(args)) {
					case ::(v, args): (new Tuple(List.rev(args), v));
					case _: assert False;
					};
					var first = ref(True);
					List.iter(function f:
					if (first.val) {
					first.val = False;
				} else {
					write(w, ", ");
					};
					expr_s(w, f), args);
					write(w, "] = ");
					expr_s(w, value);
				case TCall({ eexpr = TField(ef, f) } = e, ::(ev, [])) if (String.starts_with(field_name(f), "add_")): var name =
						field_name(f);
					var propname = String.sub(name, 4, -(String.length(name), 4));
					if (is_event(gen.greal_type(ef.etype), propname)) {
						expr_s(w, ef);
						write(w, ".");
						write_field(w, propname);
						write(w, " += ");
						expr_s(w, ev);
					} else {
						do_call(w, e, ::(ev, []));
					};
				case TCall({ eexpr = TField(ef, f) } = e, ::(ev, [])) if (String.starts_with(field_name(f), "remove_")): var name =
						field_name(f);
					var propname = String.sub(name, 7, -(String.length(name), 7));
					if (is_event(gen.greal_type(ef.etype), propname)) {
						expr_s(w, ef);
						write(w, ".");
						write_field(w, propname);
						write(w, " -= ");
						expr_s(w, ev);
					} else {
						do_call(w, e, ::(ev, []));
					};
				case TCall({ eexpr = TField(ef, f) } = e, []) if (String.starts_with(field_name(f), "get_")): var name = field_name(f);
					var propname = String.sub(name, 4, -(String.length(name), 4));
					if (is_extern_prop(gen.greal_type(ef.etype), propname)) {
						expr_s(w, ef);
						write(w, ".");
						write_field(w, propname);
					} else {
						do_call(w, e, []);
					};
				case TCall({ eexpr = TField(ef, f) } = e, ::(v, [])) if (String.starts_with(field_name(f), "set_")): var name = field_name(
								f);
					var propname = String.sub(name, 4, -(String.length(name), 4));
					if (is_extern_prop(gen.greal_type(ef.etype), propname)) {
						expr_s(w, ef);
						write(w, ".");
						write_field(w, propname);
						write(w, " = ");
						expr_s(w, v);
					} else {
						do_call(w, e, ::(v, []));
					};
				case TField(e, FStatic(_, cf) | FInstance(_, _, cf)) if (Meta.has(Meta.Native, cf.cf_meta)): 	function loop(meta) return {
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
				case TConst(c): switch (c) {
					case TInt(i32): write(w, Int32.to_string(i32));
					case TFloat(s): write(w, s);
						if ( = (String.get(s, -(String.length(s), 1)), '.')) {
							write(w, "0");
						} else {
							[];
						};
					case TString(s): write(w, "\"");
						write(w, escape(s));
						write(w, "\"");
					case TBool(b): write(w, if (b) {
						"true";
					} else {
						"false";
					});
					case TNull if (||(is_cs_basic_type(e.etype), is_tparam(e.etype))): write(w, "default[");
						write(w, t_s(e.etype));
						write(w, "]");
					case TNull: write(w, "null");
					case TThis: write(w, "this");
					case TSuper: write(w, "base");
					};
				case TLocal({ v_name = __sbreak__ }): write(w, "break");
				case TLocal({ v_name = __undefined__ }): write(w, t_s(TInst(runtime_cl, List.map(function _: t_dynamic,
							runtime_cl.cl_params))));
					write(w, ".undefined");
				case TLocal({ v_name = __typeof__ }): write(w, "typeof");
				case TLocal({ v_name = __sizeof__ }): write(w, "sizeof");
				case TLocal(var): write_id(w, var.v_name);
				case TField(_, FEnum(e, ef)): var s = ef.ef_name;
					print(w, "%s.", ^ ("global::", module_s(TEnumDecl(e))));
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
				case TField({ eexpr = TTypeExpr(mt) }, s):
					switch (mt) {
					case TClassDecl({ cl_path = (::(haxe, []), Int64) }): write(w, ^ ("global::", module_s(mt)));
					case TClassDecl({ cl_path = (::(haxe, []), Int32) }): write(w, ^ ("global::", module_s(mt)));
					case TClassDecl({ cl_interface = True }): write(w, ^ ("global::", module_s(mt)));
						write(w, "__Statics_");
					case TClassDecl(cl): write(w, t_s(TInst(cl, List.map(function _: t_empty, cl.cl_params))));
					case TEnumDecl(en): write(w, t_s(TEnum(en, List.map(function _: t_empty, en.e_params))));
					case TTypeDecl(td): write(w, t_s(gen.gfollowrun_f(TType(td, List.map(function _: t_empty, td.t_params)))));
					case TAbstractDecl(a): write(w, t_s(TAbstract(a, List.map(function _: t_empty, a.a_params))));
					};
					write(w, ".");
					write_field(w, field_name(s));
				case TField(e, s) if (is_pointer(gen, e.etype)): var e = switch (e.eexpr) {
					case TCast(e1, _) if (Gencommon.CastDetect.type_iseq(gen, e.etype, e1.etype)): e1;
					case _: e;
					};
					expr_s(w, e);
					write(w, "->");
					write_field(w, field_name(s));
				case TField(e, s): expr_s(w, e);
					write(w, ".");
					write_field(w, field_name(s));
				case TTypeExpr(mt): switch (mt) {
					case TClassDecl({ cl_path = (::(haxe, []), Int64) }): write(w, ^ ("global::", module_s(mt)));
					case TClassDecl({ cl_path = (::(haxe, []), Int32) }): write(w, ^ ("global::", module_s(mt)));
					case TClassDecl(cl): write(w, t_s(TInst(cl, List.map(function _: t_empty, cl.cl_params))));
					case TEnumDecl(en): write(w, t_s(TEnum(en, List.map(function _: t_empty, en.e_params))));
					case TTypeDecl(td): write(w, t_s(gen.gfollowrun_f(TType(td, List.map(function _: t_empty, td.t_params)))));
					case TAbstractDecl(a): write(w, t_s(TAbstract(a, List.map(function _: t_empty, a.a_params))));
					};
				case TParenthesis(e): write(w, "[");
					expr_s(w, e);
					write(w, "]");
				case TMeta(_, e): expr_s(w, e);
				case TArrayDecl(el) | TCall({ eexpr = TLocal({ v_name = __array__ }) }, el) | TCall({ eexpr = TField(_, FStatic({ cl_path = (::(cs, []), NativeArray) }, { cf_name = make })) }, el)
						: var Tuple(_, el) = extract_tparams([], el);
					print(w, "new %s", t_s(e.etype));
					write(w, "{");
					ignore(List.fold_left(function acc: function e:
					if (<>(acc, 0)) {
					write(w, ", ");
					} else {
						[];
					};
					expr_s(w, e);
					+(acc, 1), 0, el));
					write(w, "}");
				case TCall({ eexpr = TLocal({ v_name = __delegate__ }) }, ::(del, [])): expr_s(w, del);
				case TCall({ eexpr = TLocal({ v_name = __is__ }) }, ::(expr, ::({ eexpr = TTypeExpr(md) }, []))): write(w, "[ ");
					expr_s(w, expr);
					write(w, " is ");
					write(w, md_s(md));
					write(w, " ]");
				case TCall({ eexpr = TLocal({ v_name = __as__ }) }, ::(expr, ::({ eexpr = TTypeExpr(md) }, []))): write(w, "[ ");
					expr_s(w, expr);
					write(w, " as ");
					write(w, md_s(md));
					write(w, " ]");
				case TCall({ eexpr = TLocal({ v_name = __as__ }) }, ::(expr, _)): write(w, "[ ");
					expr_s(w, expr);
					write(w, " as ");
					write(w, t_s(e.etype));
					write(w, " ]");
				case TCall({ eexpr = TLocal({ v_name = __cs__ }) }, ::({ eexpr = TConst(TString(s)) }, [])): write(w, s);
				case TCall({ eexpr = TLocal({ v_name = __cs__ }) }, ::({ eexpr = TConst(TString(s)) }, tl)): Codegen.interpolate_code(
						gen.gcon, s, tl, write(w), expr_s(w), e.epos);
				case TCall({ eexpr = TLocal({ v_name = __stackalloc__ }) }, ::(e, [])): write(w, "stackalloc byte[");
					expr_s(w, e);
					write(w, "]");
				case TCall({ eexpr = TLocal({ v_name = __unsafe__ }) }, ::(e, [])): write(w, "unsafe");
					expr_s(w, mk_block(e));
				case TCall({ eexpr = TLocal({ v_name = __checked__ }) }, ::(e, [])): write(w, "checked");
					expr_s(w, mk_block(e));
				case TCall({ eexpr = TLocal({ v_name = __lock__ }) }, ::(eobj, ::(eblock, []))): write(w, "lock[");
					expr_s(w, eobj);
					write(w, "]");
					expr_s(w, mk_block(eblock));
				case TCall({ eexpr = TLocal({ v_name = __fixed__ }) }, ::(e, [])): var fixeds = ref([]);
					function loop(match) return switch (match) {
					case ::( {
										 eexpr = TVar(v, Some(e))
							} = expr, tl) if (is_pointer(gen, v.v_type)): var e = switch (get_ptr(e)) {
						case None: e;
						case Some(e): e;
						};
						fixeds.val = ::((new Tuple(v, e, expr)), fixeds.val);
						loop(tl);
					case el if (<>(fixeds.val, [])): 	function loop(fx, acc) return {
							switch (fx) {
							case ::((v, e, expr), tl): write(w, "fixed[");
								var vf = mk_temp(gen, "fixed", v.v_type);
								expr_s(w, { (expr) with eexpr = TVar(vf, Some(e)) });
								write(w, "] ");
								begin_block(w);
								expr_s(w, { (expr) with eexpr = TVar(v, Some(mk_local(vf, expr.epos))) });
								write(w, ";");
								newline(w);
								loop(tl, +(acc, 1));
							case []: acc;
							};
						};
						var nblocks = loop(List.rev(fixeds.val), 0);
						in_value.val = False;
						expr_s(w, { (e) with eexpr = TBlock(el) });
						for (i in /*to*/1...nblocks) {
							end_block(w);
						};
					case _: trace(debug_expr(e));
						gen.gcon.error("Invalid 'fixed' keyword format", e.epos);
					};
					switch (e.eexpr) {
					case TBlock(bl): loop(bl);
					case _: trace("not block");
						trace(debug_expr(e));
						gen.gcon.error("Invalid 'fixed' keyword format", e.epos);
					};
				case TCall({ eexpr = TLocal({ v_name = __addressOf__ }) }, ::(e, [])): var e = ensure_local(e, "for addressOf");
					write(w, "&");
					expr_s(w, e);
				case TCall({ eexpr = TLocal({ v_name = __valueOf__ }) }, ::(e, [])): write(w, "*[");
					expr_s(w, e);
					write(w, "]");
				case TCall({ eexpr = TLocal({ v_name = __goto__ }) }, ::({ eexpr = TConst(TInt(v)) }, [])): print(w, "goto label%ld", v);
				case TCall({ eexpr = TLocal({ v_name = __label__ }) }, ::({ eexpr = TConst(TInt(v)) }, [])): print(w, "label%ld: {}", v);
				case TCall({ eexpr = TLocal({ v_name = __rethrow__ }) }, _): write(w, "throw");
				case TCall({ eexpr = TField(ef, FInstance(cl, _, { cf_name = __get })) }, ::(idx, [])) if (!(is_hxgen(TClassDecl(cl)))):
					expr_s(w, { (e) with eexpr = TArray(ef, idx) });
				case TCall({ eexpr = TField(ef, FInstance(cl, _, { cf_name = __set })) }, ::(idx, ::(v, []))) if (!(is_hxgen(TClassDecl(cl))))
							: expr_s(w, { (e) with eexpr = TBinop(Ast.OpAssign, { (e) with eexpr = TArray(ef, idx) }, v) });
				case TCall({ eexpr = TField(ef, FStatic(_, cf)) }, el) if (PMap.mem(cf.cf_name, binops_names)): var Tuple(_,
							elr) = extract_tparams([], el);
					switch (elr) {
					case ::(e1, ::(e2, [])): expr_s(w, { (e) with eexpr = TBinop(PMap.find(cf.cf_name, binops_names), e1, e2) });
					case _: do_call(w, e, el);
					};
				case TCall({ eexpr = TField(ef, FStatic(_, cf)) }, el) if (PMap.mem(cf.cf_name, unops_names)):
					switch (extract_tparams([], el)) {
					case (_, ::(e1, [])): expr_s(w, { (e) with eexpr = TUnop(PMap.find(cf.cf_name, unops_names), Ast.Prefix, e1) });
					case _: do_call(w, e, el);
					};
				case TCall(e, el): do_call(w, e, el);
				case TNew({ cl_path = (::(cs, []), NativeArray) } = cl, params, ::(size, [])): 	function check_t_s(t, times) return {
						switch (real_type(t)) {
						case TInst({ cl_path = (::(cs, []), NativeArray) }, ::(param, [])): check_t_s(param, +(times, 1));
						case _: print(w, "new %s[", t_s(run_follow(gen, t)));
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
					write(w, t_s(TInst(cl, [])));
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
							"default[%s] /* This code should never be reached. It was produced by the use of @:generic on a new type parameter instance: %s */",
							t_s(TInst(cl, params)), path_param_s(TClassDecl(cl), cl.cl_path, params));
				case TNew(cl, params, el): write(w, "new ");
					write(w, path_param_s(TClassDecl(cl), cl.cl_path, params));
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
				case TVar(var, eopt): print(w, "%s ", t_s(var.v_type));
					write_id(w, var.v_name);
					switch (eopt) {
					case None: write(w, " = ");
						expr_s(w, null(var.v_type, e.epos));
					case Some(e): write(w, " = ");
						expr_s(w, e);
					};
				case TBlock(::(e, [])) if (was_in_value): expr_s(w, e);
				case TBlock(el): begin_block(w);
					List.iter(function e: List.iter(function e: line_directive(w, e.epos);
													in_value.val = False;
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
					case Some(e): write(w, "else ");
						in_value.val = False;
						var e = switch (e.eexpr) {
						case TIf(_): e;
						case TBlock(::({ eexpr = TIf(_) } = e, [])): e;
						case _: mk_block(e);
						};
						expr_s(w, e);
					};
				case TWhile(econd, eblock, flag): switch (flag) {
					case Ast.NormalWhile: write(w, "while ");
						expr_s(w, mk_paren(econd));
						write(w, " ");
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
					write(w, " ");
					begin_block(w);
					List.iter(function (el, e): List.iter(function e: write(w, "case ");
														  in_value.val = True;
														  expr_s(w, e);
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
					List.iter(function (var, e): print(w, "catch [%s %s]", t_s(var.v_type), var.v_name);
							  in_value.val = False;
							  expr_s(w, mk_block(e));
							  newline(w), ve_l);
				case TReturn(eopt): write(w, "return");
					if (is_some(eopt)) {
						write(w, " ");
						expr_s(w, get(eopt));
					} else {
						[];
					};
				case TBreak: write(w, "break");
				case TContinue: write(w, "continue");
				case TThrow(e): write(w, "throw ");
					expr_s(w, e);
				case TCast(e1, md_t): print(w, "[[%s] [", t_s(e.etype));
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
			function do_call(w, e, el) return {
				var Tuple(params, el) = extract_tparams([], el);
				var params = List.rev(params);
				expr_s(w, e);
				switch (params) {
				case ::(_, _) if (!(&&(erase_generics, field_is_hxgeneric(e)))): var md = switch (e.eexpr) {
					case TField(ef, _): t_to_md(run_follow(gen, ef.etype));
					case _: assert False;
					};
					write(w, "<");
					ignore(List.fold_left(function acc: function t:
					if (<>(acc, 0)) {
					write(w, ", ");
					} else {
						[];
					};
					write(w, t_s(t));
					+(acc, 1), 0, change_param_type(md, params)));
					write(w, ">");
				case _: [];
				};
				function loop(acc, elist, tlist) return {
					switch ((new Tuple(elist, tlist))) {
					case (::(e, etl), ::((_, _, t), ttl)): if (<>(acc, 0)) {
							write(w, ", ");
						} else {
							[];
						};
						switch (real_type(t)) {
						case TType({ t_path = (::(cs, []), Ref) }, _) | TAbstract({ a_path = (::(cs, []), Ref) }, _): var e = ensure_refout(e,
									"of type cs.Ref");
							write(w, "ref ");
							expr_s(w, e);
						case TType({ t_path = (::(cs, []), Out) }, _) | TAbstract({ a_path = (::(cs, []), Out) }, _): var e = ensure_refout(e,
									"of type cs.Out");
							write(w, "out ");
							expr_s(w, e);
						case _: expr_s(w, e);
						};
						loop(+(acc, 1), etl, ttl);
					case (::(e, etl), []): if (<>(acc, 0)) {
							write(w, ", ");
						} else {
							[];
						};
						expr_s(w, e);
						loop(+(acc, 1), etl, []);
					case _: [];
					};
				};
				write(w, "[");
				var ft = switch (follow(e.etype)) {
				case TFun(args, _): args;
				case _: [];
				};
				loop(0, el, ft);
				write(w, "]");
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
		case (EArrayDecl(el), _): write(w, "new[] {");
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
		function gen_attributes(w, metadata) return {
		List.iter(function case (Meta.Meta, ::((EConst(String(s)), _), []), _): write(w, "[");
				write(w, s);
				write(w, "]");
				newline(w);
			case (Meta.Meta, ::(meta, []), _): write(w, "[");
					gen_spart(w, meta);
					write(w, "]");
					newline(w);
				case _: [], metadata);
		};
		function gen_nocompletion(w, metadata) return {
			if (Meta.has(Meta.NoCompletion, metadata)) {
				write(w, "[global::System.ComponentModel.EditorBrowsable[global::System.ComponentModel.EditorBrowsableState.Never]]");
				newline(w);
			} else {
				[];
			};
		};
		function argt_s(t) return {
			var w = new_source_writer([]);
			function run(t) return {
				switch (t) {
				case TType(tdef, p): gen_attributes(w, tdef.t_meta);
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
			var ret = switch (run_follow(gen, t)) {
			case TType({ t_path = (::(cs, []), Ref) }, ::(t, [])) | TAbstract({ a_path = (::(cs, []), Ref) }, ::(t, [])): ^ ("ref ",
						t_s(t));
			case TType({ t_path = (::(cs, []), Out) }, ::(t, [])) | TAbstract({ a_path = (::(cs, []), Out) }, ::(t, [])): ^ ("out ",
						t_s(t));
			case t: t_s(t);
			};
			var c = contents(w);
			if (<>(c, "")) {
				^ (c, ^ (" ", ret));
			} else {
				ret;
			};
		};
		function get_string_params(cl, cl_params) return {
			var hxgen = is_hxgen(TClassDecl(cl));
			switch (cl_params) {
			case ::(_, _) if (!(&&(erase_generics, is_hxgeneric(TClassDecl(cl))))): 	function get_param_name(t) return {
					switch (follow(t)) {
					case TInst(cl, _): snd(cl.cl_path);
					case _: assert False;
					};
				};
				var params = sprintf("<%s>", String.concat(", ", List.map(function (_, tcl): get_param_name(tcl), cl_params)));
				var params_extends = if ( || (hxgen, !(Meta.has(Meta.Custom(":nativeTypeConstraints"), cl.cl_meta)))) {
					::("", []);
				} else {
					List.fold_left(function acc: function (name, t):
					switch (run_follow(gen, t)) {
				case TInst({ cl_kind = KTypeParameter(constraints) }, _) if (<>(constraints, [])): var base_class_constraints = ref([]);
						var other_constraints = List.fold_left(function acc: function t:
						switch (follow(t)) {
					case TInst({ cl_path = ([], String) }, []): acc;
						case TInst({ cl_interface = False; cl_meta = meta }, _) if (!(Meta.has(Meta.Final, meta))): base_class_constraints.val
								= ::(t_s(t), base_class_constraints.val);
							acc;
						case TInst({ cl_interface = True }, _): ::(t_s(t), acc);
						case _: acc;
						}, [], constraints);
						var s_constraints = @(base_class_constraints.val, other_constraints);
						if (<>(s_constraints, [])) {
							::(sprintf(" where %s : %s", get_param_name(t), String.concat(", ", s_constraints)), acc);
						} else {
							acc;
						};
					case _: acc;
					}, [], cl_params);
				};
				(new Tuple(params, String.concat(" ", params_extends)));
			case _: (new Tuple("", ""));
			};
		};
		function gen_field_decl(w, visibility, v_n, modifiers, t, n) return {
			var parts = ref([]);
			if (<>(visibility, "")) {
				parts.val = ::(visibility, parts.val);
			} else {
				[];
			};
			if (<>(v_n, "")) {
				parts.val = ::(v_n, parts.val);
			} else {
				[];
			};
			if (<>(modifiers, [])) {
				parts.val = @(modifiers, parts.val);
			} else {
				[];
			};
			if (<>(t, "")) {
				parts.val = ::(t, parts.val);
			} else {
				[];
			};
			parts.val = ::(n, parts.val);
			write(w, String.concat(" ", List.rev(parts.val)));
		};
		function gen_event(w, is_static, cl, Tuple(event, t, custom, add, remove)) return {
			var is_interface = cl.cl_interface;
			var visibility = if (is_interface) {
				"";
			} else {
				"public";
			};
			var Tuple(visibility, modifiers) = get_fun_modifiers(event.cf_meta, visibility, ::("event", []));
			var v_n = if (is_static) {
				"static";
			} else {
				"";
			};
			gen_field_decl(w, visibility, v_n, modifiers, t_s(run_follow(gen, t)), change_field(event.cf_name));
			if ( && (custom, !(is_interface))) {
				write(w, " ");
				begin_block(w);
				print(w, "add { _add_%s[value]; }", event.cf_name);
				newline(w);
				print(w, "remove { _remove_%s[value]; }", event.cf_name);
				newline(w);
				end_block(w);
				newline(w);
			} else {
				write(w, ";\n");
			};
			newline(w);
		};
		function gen_prop(w, is_static, cl, is_final, Tuple(prop, t, get, set)) return {
			gen_attributes(w, prop.cf_meta);
			var is_interface = cl.cl_interface;
			function fn_is_final(match) return switch (match) {
			case None: True;
			case Some({ cf_kind = Method(mkind) } = m): || (switch (mkind) {
			case MethInline: True;
			case _: False;
			}, Meta.has(Meta.Final, m.cf_meta));
			case _: assert False;
			};
			var is_virtual = !( || (is_interface, || (is_final, || (Meta.has(Meta.Final, prop.cf_meta), || (fn_is_final(get), fn_is_final(set))))));
			function fn_is_override(match) return switch (match) {
			case Some(cf): List.memq(cf, cl.cl_overrides);
			case None: False;
			};
			var is_override = || (fn_is_override(get), fn_is_override(set));
			var visibility = if (is_interface) {
				"";
			} else {
				"public";
			};
			var Tuple(visibility, modifiers) = get_fun_modifiers(prop.cf_meta, visibility, []);
			var v_n = if (is_static) {
				"static";
			} else {
				if ( && (is_override, !(is_interface))) {
					"override";
				} else {
					if (is_virtual) {
						"virtual";
					} else {
						"";
					};
				};
			};
			gen_nocompletion(w, prop.cf_meta);
			gen_field_decl(w, visibility, v_n, modifiers, t_s(run_follow(gen, t)), change_field(prop.cf_name));
			function check(cf) return {
				switch (cf) {
				case Some({ cf_overloads = ::(o, _) } = cf):
					gen.gcon.error("Property functions with more than one overload is currently unsupported", cf.cf_pos);
					gen.gcon.error("Property functions with more than one overload is currently unsupported", o.cf_pos);
				case _: [];
				};
			};
			check(get);
			check(set);
			write(w, " ");
			if (is_interface) {
				write(w, "{ ");
				var s = ref("");
				switch (prop.cf_kind) {
				case Var({ v_read = AccCall }): write(w, "get;");
					s.val = " ";
				case _: [];
				};
				switch (prop.cf_kind) {
				case Var({ v_write = AccCall }): print(w, "%sset;", s.val);
				case _: [];
				};
				write(w, " }");
				newline(w);
			} else {
				begin_block(w);
				switch (get) {
				case Some(cf): print(w, "get { return _get_%s[]; }", prop.cf_name);
					newline(w);
					cf.cf_meta = ::((new Tuple(Meta.Custom("?prop_impl"), [], null_pos)), cf.cf_meta);
				case None: [];
				};
				switch (set) {
				case Some(cf): print(w, "set { _set_%s[value]; }", prop.cf_name);
					newline(w);
					cf.cf_meta = ::((new Tuple(Meta.Custom("?prop_impl"), [], null_pos)), cf.cf_meta);
				case None: [];
				};
				end_block(w);
				newline(w);
				newline(w);
			};
		};
		function gen_class_field(w, ? : (is_overload = False), is_static, cl, is_final, cf) return {
			gen_attributes(w, cf.cf_meta);
			var is_interface = cl.cl_interface;
			var Tuple(name, is_new, is_explicit_iface) = switch (cf.cf_name) {
			case new: (new Tuple(snd(cl.cl_path), True, False));
			case name if (String.contains(name, '.')): var Tuple(fn_name, path) = parse_explicit_iface(name);
				(new Tuple( ^ (path_s(path), ^ (".", fn_name)), False, True));
			case name: try {
					var binop = PMap.find(name, binops_names);
					(new Tuple( ^ ("operator ", s_binop(binop)), False, False));
				} catch (e: Not_found) {
					try {
						var unop = PMap.find(name, unops_names);
						(new Tuple( ^ ("operator ", s_unop(unop)), False, False));
					} catch (e: Not_found) {
						if ( || (Meta.has(Meta.Custom("?prop_impl"), cf.cf_meta), Meta.has(Meta.Custom("?event_impl"), cf.cf_meta))) {
							(new Tuple( ^ ("_", name), False, False));
						} else {
							(new Tuple(name, False, False));
						};
					};
				};
			};
			function loop_static(cl) return {
				switch ((new Tuple(is_static, cl.cl_super))) {
				case (False, _): [];
				case (True, None): [];
				case (True, Some(cl, _)): try {
						var cf2 = PMap.find(cf.cf_name, cl.cl_statics);
						Gencommon.CastDetect.type_eq(gen, EqStrict, cf.cf_type, cf2.cf_type);
						::("new", []);
					} catch (e: Not_found | Unify_error(_)) {
						loop_static(cl);
					};
				};
			};
			var modf = loop_static(cl);
			switch (cf.cf_kind) {
			case Var(_) | Method(MethDynamic) if (!(Type.is_extern_field(cf))):
				if ( || (is_overload, List.exists(function cf: <>(cf.cf_expr, None), cf.cf_overloads))) {
					gen.gcon.error("Only normal [non-dynamic] methods can be overloaded", cf.cf_pos);
				} else {
					[];
				};
				if (!(is_interface)) {
					var Tuple(access, modifiers) = get_fun_modifiers(cf.cf_meta, "public", []);
					var modifiers = @(modifiers, modf);
					gen_nocompletion(w, cf.cf_meta);
					gen_field_decl(w, access, if (is_static) {
					"static";
				} else {
					"";
				}, modifiers, t_s(run_follow(gen, cf.cf_type)), change_field(name));
					switch (cf.cf_expr) {
					case Some(e): write(w, " = ");
						expr_s(w, e);
					case None: [];
					};
					write(w, ";");
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
				var is_virtual = && (!(is_final), switch (mkind) {
			case MethInline: False;
			case _ if (!(is_new)): True;
				case _: False;
				});
				var is_virtual = if ( || (!(is_virtual), Meta.has(Meta.Final, cf.cf_meta))) {
					False;
				} else {
					is_virtual;
				};
				var is_override = List.memq(cf, cl.cl_overrides);
				var is_override = || (is_override, switch ((new Tuple(cf.cf_name, follow(cf.cf_type)))) {
			case (Equals, TFun(::((_, _, targ), []), tret)): switch ((new Tuple(follow(targ), follow(tret)))) {
					case (TDynamic(_), TAbstract({ a_path = ([], Bool) }, [])): True;
					case _: False;
					};
				case (GetHashCode, TFun([], _)): True;
				case _: False;
				});
				var is_override = if (Meta.has(Meta.Custom("?prop_impl"), cf.cf_meta)) {
					False;
				} else {
					is_override;
				};
				var is_virtual = && (is_virtual, && (!(Meta.has(Meta.Final, cl.cl_meta)), !(is_interface)));
				var visibility = if (is_interface) {
					"";
				} else {
					"public";
				};
				var Tuple(visibility, modifiers) = get_fun_modifiers(cf.cf_meta, visibility, []);
				var modifiers = @(modifiers, modf);
				var Tuple(visibility, is_virtual) = if (is_explicit_iface) {
					(new Tuple("", False));
				} else {
					if ( = (visibility, "private")) {
						(new Tuple("private", False));
					} else {
						(new Tuple(visibility, is_virtual));
					};
				};
				var v_n = if (is_static) {
					"static";
				} else {
					if ( && (is_override, !(is_interface))) {
						"override";
					} else {
						if (is_virtual) {
							"virtual";
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
				var Tuple(ret_type, args) = switch (follow(cf_type)) {
				case TFun(strbtl, t): (new Tuple(t, strbtl));
				case _: assert False;
				};
				gen_nocompletion(w, cf.cf_meta);
				gen_field_decl(w, visibility, v_n, modifiers, if (!(is_new)) {
				rett_s(run_follow(gen, ret_type));
				} else {
					"";
				}, change_field(name));
				var Tuple(params, params_ext) = get_string_params(cl, cf.cf_params);
				switch (cf.cf_expr) {
				case Some({ eexpr = TFunction(tf) }): print(w, "%s[%s]%s", params, String.concat(", ", List.map2(function (var,
							_): function (_, _, t): sprintf("%s %s", argt_s(t), change_id(var.v_name)), tf.tf_args, args)), params_ext);
				case _: print(w, "%s[%s]%s", params, String.concat(", ", List.map(function (name, _, t): sprintf("%s %s", argt_s(t),
								  change_id(name)), args)), params_ext);
				};
				if (is_interface) {
					write(w, ";");
				} else {
					write(w, " ");
					function loop(meta) return {
						switch (meta) {
						case []: var expr = switch (cf.cf_expr) {
							case None: mk(TBlock([]), t_dynamic, Ast.null_pos);
							case Some(s): switch (s.eexpr) {
								case TFunction(tf): mk_block(tf.tf_expr);
								case _: assert False;
								};
							};
							function needs_unchecked(e) return {
								function loop(e) return {
									switch (e.eexpr) {
									case TConst(TInt(i)) if (<>(i, Int32.zero)): raise(Exit);
									case TCall({ eexpr = TLocal({ v_name = __checked__ }) }, _): [];
									case TNew({ cl_path = (::(haxe, ::(lang, [])), DynamicObject) }, [], ::(_, ::(e1, ::(_, ::(e2, []))))): loop(e1);
										loop(e2);
									case TNew({ cl_path = (::(haxe, ::(lang, [])), Closure) }, [], ::(eo, ::(_, ::(_, [])))): loop(eo);
									case TCall({ eexpr = TField(_, FStatic({ cl_path = (::(haxe, ::(lang, [])), Runtime) }, { cf_name = getField | setField | getField_f | setField_f | callField })) }, ::(eo, ::(_, ::(_, rest))))
											: loop(eo);
										List.iter(loop, rest);
									case _: Type.iter(loop, e);
									};
								};
								try {
									loop(e);
									False;
								} catch (e: Exit) {
									True;
								};
							};
							function write_method_expr(e) return {
								switch (e.eexpr) {
								case TBlock([]): begin_block(w);
									end_block(w);
								case TBlock(_): var unchecked = needs_unchecked(e);
									if (unchecked) {
										begin_block(w);
										write(w, "unchecked ");
									} else {
										[];
									};
									var t = Common.timer("expression to string");
									expr_s(w, e);
									t([]);
									line_reset_directive(w);
									if (unchecked) {
										end_block(w);
									} else {
										[];
									};
								case _: assert False;
								};
							};
							if (is_new) {
								function get_super_call(el) return {
									switch (el) {
									case ::( {
											eexpr = TCall({ eexpr = TConst(TSuper) }, _)
										} = call, rest): (new Tuple(Some(call), rest));
									case ::( {
														 eexpr = TBlock(bl)
													 } = block, rest): var Tuple(ret, mapped) = get_super_call(bl);
										(new Tuple(ret, ::({ (block) with eexpr = TBlock(mapped) }, rest)));
									case _: (new Tuple(None, el));
									};
								};
								switch (expr.eexpr) {
								case TBlock(bl): var Tuple(super_call, rest) = get_super_call(bl);
									switch (super_call) {
									case None: [];
									case Some(sc): write(w, ": ");
										var t = Common.timer("expression to string");
										expr_s(w, sc);
										write(w, " ");
										t([]);
									};
									write_method_expr({ (expr) with eexpr = TBlock(rest) });
								case _: assert False;
								};
							} else {
								write_method_expr(expr);
							};
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
		function check_special_behaviors(w, cl) return {
			switch (cl.cl_kind) {
			case KAbstractImpl(_): [];
			case _: var pairs = ref(PMap.empty);
				try {
					var get = PMap.find("__get", cl.cl_fields);
					List.iter(function cf: var Tuple(args, ret) = get_fun(cf.cf_type);
					switch (args) {
				case ::((_, _, idx), []): pairs.val = PMap.add(t_s(idx), (new Tuple(t_s(ret), Some(cf), None)), pairs.val);
					case _: gen.gcon.warning("The __get function must have exactly one argument [the index]", cf.cf_pos);
					}, ::(get, get.cf_overloads));
				} catch (e: Not_found) {
					[];
				};
				try {
					var set = PMap.find("__set", cl.cl_fields);
					List.iter(function cf: var Tuple(args, ret) = get_fun(cf.cf_type);
					switch (args) {
				case ::((_, _, idx), ::((_, _, v), [])): try {
							var Tuple(vt, g, _) = PMap.find(t_s(idx), pairs.val);
							var tvt = t_s(v);
							if (<>(vt, tvt)) {
								gen.gcon.warning("The __get function of same index has a different type from this __set function", cf.cf_pos);
							} else {
								[];
							};
							pairs.val = PMap.add(t_s(idx), (new Tuple(vt, g, Some(cf))), pairs.val);
						} catch (e: Not_found) {
							pairs.val = PMap.add(t_s(idx), (new Tuple(t_s(v), None, Some(cf))), pairs.val);
						};
					case _: gen.gcon.warning("The __set function must have exactly two arguments [index, value]", cf.cf_pos);
					}, ::(set, set.cf_overloads));
				} catch (e: Not_found) {
					[];
				};
				PMap.iter(function idx: function (v, get, set): print(w, "public %s this[%s index]", v, idx);
						  begin_block(w);
				switch (get) {
			case None: [];
				case Some(_): write(w, "get");
					begin_block(w);
					write(w, "return this.__get[index];");
					end_block(w);
				};
				switch (set) {
			case None: [];
				case Some(_): write(w, "set");
					begin_block(w);
					write(w, "this.__set[index,value];");
					end_block(w);
				};
				end_block(w), pairs.val);
				if (!(PMap.is_empty(pairs.val))) {
					try {
						var get = PMap.find("__get", cl.cl_fields);
						var Tuple(idx_t, v_t) = switch (follow(get.cf_type)) {
						case TFun(::((_, _, arg_t), []), ret_t): (new Tuple(t_s(run_follow(gen, arg_t)), t_s(run_follow(gen, ret_t))));
						case _: gen.gcon.error("The __get function must be a function with one argument. ", get.cf_pos);
							assert False;
						};
						List.iter(function (cl, args):
						switch (cl.cl_array_access) {
					case None: [];
						case Some(t): var changed_t = apply_params(cl.cl_params, List.map(function _: t_dynamic, cl.cl_params), t);
							var t_as_s = t_s(run_follow(gen, changed_t));
							print(w, "%s %s.this[int key]", t_as_s, t_s(TInst(cl, args)));
							begin_block(w);
							write(w, "get");
							begin_block(w);
							print(w, "return [[%s] this.__get[key]];", t_as_s);
							end_block(w);
							write(w, "set");
							begin_block(w);
							print(w, "this.__set[key, [%s] value];", v_t);
							end_block(w);
							end_block(w);
							newline(w);
							newline(w);
						}, cl.cl_implements);
					} catch (e: Not_found) {
						[];
					};
				} else {
					[];
				};
				if ( && (cl.cl_interface, && (is_hxgen(TClassDecl(cl)), is_some(cl.cl_array_access)))) {
					var changed_t = apply_params(cl.cl_params, List.map(function _: t_dynamic, cl.cl_params), get(cl.cl_array_access));
					print(w, "%s this[int key]", t_s(run_follow(gen, changed_t)));
					begin_block(w);
					write(w, "get;");
					newline(w);
					write(w, "set;");
					newline(w);
					end_block(w);
					newline(w);
					newline(w);
				} else {
					[];
				};
				try {
					if (cl.cl_interface) {
						raise(Not_found);
					} else {
						[];
					};
					var cf = PMap.find("toString", cl.cl_fields);
					if (List.exists(function c: = (c.cf_name, "toString"), cl.cl_overrides)) {
						raise(Not_found);
					} else {
						[];
					};
					switch (cf.cf_type) {
					case TFun([], ret): switch (real_type(ret)) {
						case TInst({ cl_path = ([], String) }, []): write(w, "public override string ToString[]");
							begin_block(w);
							write(w, "return this.toString[];");
							end_block(w);
							newline(w);
							newline(w);
						case _: gen.gcon.error("A toString[] function should return a String!", cf.cf_pos);
						};
					case _: [];
					};
				} catch (e: Not_found) {
					[];
				};
				try {
					if (cl.cl_interface) {
						raise(Not_found);
					} else {
						[];
					};
					var cf = PMap.find("finalize", cl.cl_fields);
					if (List.exists(function c: = (c.cf_name, "finalize"), cl.cl_overrides)) {
						raise(Not_found);
					} else {
						[];
					};
					switch (cf.cf_type) {
					case TFun([], ret): switch (real_type(ret)) {
						case TAbstract({ a_path = ([], Void) }, []): write(w, "~");
							write(w, snd(cl.cl_path));
							write(w, "[]");
							begin_block(w);
							write(w, "this.finalize[];");
							end_block(w);
							newline(w);
							newline(w);
						case _: gen.gcon.error("A finalize[] function should be Void->Void!", cf.cf_pos);
						};
					case _: [];
					};
				} catch (e: Not_found) {
					[];
				};
				function handle_prop(static, f) return {
					switch (f.cf_kind) {
					case Method(_): [];
					case Var(v) if (!(Type.is_extern_field(f))): [];
					case Var(v): 	function prop(acc) return {
							switch (acc) {
							case AccNo | AccNever | AccCall: True;
							case _: False;
							};
						};
						if ( && (prop(v.v_read), && (prop(v.v_write), || ( = (v.v_read, AccCall), = (v.v_write, AccCall))))) {
							var this = if (static) {
								mk_classtype_access(cl, f.cf_pos);
							} else {
								{
									() with eexpr = TConst(TThis);
									etype = TInst(cl, List.map(snd, cl.cl_params));
									epos = f.cf_pos
								};
							};
							print(w, "public %s%s %s", if (static) {
							"static ";
						} else {
							"";
						}, t_s(f.cf_type), netname_to_hx(f.cf_name));
							begin_block(w);
							switch (v.v_read) {
							case AccCall: write(w, "get");
								begin_block(w);
								write(w, "return ");
								expr_s(w, this);
								print(w, ".get_%s[];", f.cf_name);
								end_block(w);
							case _: [];
							};
							switch (v.v_write) {
							case AccCall: write(w, "set");
								begin_block(w);
								expr_s(w, this);
								print(w, ".set_%s[value];", f.cf_name);
								end_block(w);
							case _: [];
							};
							end_block(w);
						} else {
							[];
						};
					};
				};
				if (Meta.has(Meta.BridgeProperties, cl.cl_meta)) {
					List.iter(handle_prop(True), cl.cl_ordered_statics);
					List.iter(handle_prop(False), cl.cl_ordered_fields);
				} else {
					[];
				};
			};
		};
		function gen_class(w, cl) return {
			write(w, "#pragma warning disable 109, 114, 219, 429, 168, 162");
			newline(w);
			var should_close = switch (change_ns(TClassDecl(cl), fst(cl.cl_path))) {
			case []: False;
			case ns: print(w, "namespace %s ", String.concat(".", ns));
				begin_block(w);
				True;
			};
			try {
				var Tuple(_, m, _) = Meta.get(Meta.Custom("generic_iface"), cl.cl_meta);
				function loop(i, acc) return {
					if ( == (i, 0)) {
						acc;
					} else {
						::("object", loop(pred(i), acc));
					};
				};
				var tparams = loop(switch (m) {
			case ::((EConst(Int(s)), _), []): int_of_string(s);
				case _: assert False;
				}, []);
				cl.cl_meta = ::((new Tuple(Meta.Meta, ::((new Tuple(EConst(String( ^ ("global::haxe.lang.GenericInterface[typeof[global::", ^ (module_s(TClassDecl(cl)), ^ ("<", ^ (String.concat(", ", tparams), ">]]")))))), cl.cl_pos)), []), cl.cl_pos)), cl.cl_meta);
			} catch (e: Not_found) {
				[];
			};
			gen_attributes(w, cl.cl_meta);
			var is_main = switch (gen.gcon.main_class) {
			case Some((_, Main) = path) if (&&(=(path, cl.cl_path), !(cl.cl_interface))): write(w, "public class EntryPoint__Main ");
				begin_block(w);
				write(w, "public static void Main[] ");
				begin_block(w);
				if (Hashtbl.mem(gen.gtypes, (new Tuple(::("cs", []), "Boot")))) {
					write(w, "global::cs.Boot.init[];");
				} else {
					[];
				};
				newline(w);
				expr_s(w, { () with eexpr = TTypeExpr(TClassDecl(cl));
							etype = t_dynamic;
							epos = Ast.null_pos
						  });
				write(w, ".main[];");
				end_block(w);
				end_block(w);
				newline(w);
				False;
			case Some(path) if (&&(=(path, cl.cl_path), !(cl.cl_interface))): True;
			case _: False;
			};
			var Tuple(clt, access, modifiers) = get_class_modifiers(cl.cl_meta, if (cl.cl_interface) {
			"interface";
		} else {
			"class";
		}, "public", []);
			var is_final = || ( = (clt, "struct"), Meta.has(Meta.Final, cl.cl_meta));
			var modifiers = @(::(access, []), modifiers);
			print(w, "%s %s %s", String.concat(" ", modifiers), clt, change_clname(snd(cl.cl_path)));
			var Tuple(params, params_ext) = get_string_params(cl, cl.cl_params);
			var extends_implements = @(switch (cl.cl_super) {
		case None: [];
			case Some(cl, p): ::(path_param_s(TClassDecl(cl), cl.cl_path, p), []);
			}, List.map(function (cl, p): path_param_s(TClassDecl(cl), cl.cl_path, p), cl.cl_implements));
			switch (extends_implements) {
			case []: print(w, "%s%s ", params, params_ext);
			case _: print(w, "%s : %s%s ", params, String.concat(", ", extends_implements), params_ext);
			};
			begin_block(w);
			newline(w);
			function loop(meta) return {
				switch (meta) {
				case []: [];
				case ::((Meta.ClassCode, ::((Ast.EConst(Ast.String(contents)), _), []), _), tl): write(w, contents);
				case ::(_, tl): loop(tl);
				};
			};
			loop(cl.cl_meta);
			if (is_main) {
				write(w, "public static void Main[]");
				begin_block(w);
				{
					if (Hashtbl.mem(gen.gtypes, (new Tuple(::("cs", []), "Boot")))) {
						write(w, "global::cs.Boot.init[];");
					} else {
						[];
					};
					newline(w);
				};
				write(w, "main[];");
				end_block(w);
			} else {
				[];
			};
			switch (cl.cl_init) {
			case None: [];
			case Some(init): print(w, "static %s[] ", snd(cl.cl_path));
				expr_s(w, mk_block(init));
				line_reset_directive(w);
				newline(w);
				newline(w);
			};
			function partition(cf, cflist) return {
				var Tuple(events, props, nonprops) = (new Tuple(ref([]), ref([]), ref([])));
				List.iter(function v:
				switch (v.cf_kind) {
			case Var({ v_read = AccCall }) | Var({ v_write = AccCall }) if (&&(Type.is_extern_field(v), Meta.has(Meta.Property, v.cf_meta)))
						: props.val = ::((new Tuple(v.cf_name, ref((new Tuple(v, v.cf_type, None, None))))), props.val);
				case Var({ v_read = AccNormal; v_write = AccNormal }) if (Meta.has(Meta.Event, v.cf_meta)):
					if (v.cf_public) {
						gen.gcon.error("@:event fields must be private", v.cf_pos);
					} else {
						[];
					};
					v.cf_meta = ::((new Tuple(Meta.SkipReflection, [], null_pos)), v.cf_meta);
					events.val = ::((new Tuple(v.cf_name, ref((new Tuple(v, v.cf_type, False, None, None))))), events.val);
				case _: nonprops.val = ::(v, nonprops.val);
				}, cflist);
				var Tuple(events, nonprops) = (new Tuple(events.val, nonprops.val));
				var t = TInst(cl, List.map(snd, cl.cl_params));
				function find_prop(name) return {
					try {
						List.assoc(name, props.val);
					} catch (e: Not_found) {
						switch (field_access(gen, t, name)) {
						case FClassField(_, _, decl, v, _, t, _) if (is_extern_prop(TInst(cl, List.map(snd, cl.cl_params)), name)): var ret = ref((
								new Tuple(v, t, None, None)));
							props.val = ::((new Tuple(name, ret)), props.val);
							ret;
						case _: raise(Not_found);
						};
					};
				};
				function find_event(name) return {
					List.assoc(name, events);
				};
				function is_empty_function(cf) return {
					switch (cf.cf_expr) {
					case Some({ eexpr = TFunction({ tf_expr = { eexpr = TBlock([]) } }) }): True;
					case _: False;
					};
				};
				var interf = cl.cl_interface;
			var nonprops = List.filter(function case cf if (String.starts_with(cf.cf_name, "get_")):
					try {
						var prop = find_prop(String.sub(cf.cf_name, 4, -(String.length(cf.cf_name), 4)));
							var Tuple(v, t, get, set) = prop.val;
							assert = (get, None);
							prop.val = (new Tuple(v, t, Some(cf), set));
							!(interf);
						} catch (e: Not_found) {
							True;
						};
			case cf if (String.starts_with(cf.cf_name, "set_")):
					try {
						var prop = find_prop(String.sub(cf.cf_name, 4, -(String.length(cf.cf_name), 4)));
							var Tuple(v, t, get, set) = prop.val;
							assert = (set, None);
							prop.val = (new Tuple(v, t, get, Some(cf)));
							!(interf);
						} catch (e: Not_found) {
							True;
						};
			case cf if (String.starts_with(cf.cf_name, "add_")):
					try {
						var event = find_event(String.sub(cf.cf_name, 4, -(String.length(cf.cf_name), 4)));
							var Tuple(v, t, _, add, remove) = event.val;
							assert = (add, None);
							cf.cf_meta = ::((new Tuple(Meta.Custom("?event_impl"), [], null_pos)), cf.cf_meta);
							var custom = !(is_empty_function(cf));
							event.val = (new Tuple(v, t, custom, Some(cf), remove));
							False;
						} catch (e: Not_found) {
							True;
						};
			case cf if (String.starts_with(cf.cf_name, "remove_")):
					try {
						var event = find_event(String.sub(cf.cf_name, 7, -(String.length(cf.cf_name), 7)));
							var Tuple(v, t, _, add, remove) = event.val;
							assert = (remove, None);
							cf.cf_meta = ::((new Tuple(Meta.Custom("?event_impl"), [], null_pos)), cf.cf_meta);
							var custom = !(is_empty_function(cf));
							event.val = (new Tuple(v, t, custom, add, Some(cf)));
							False;
						} catch (e: Not_found) {
							True;
						};
			case _: True, nonprops);
				var nonprops = ref(nonprops);
				List.iter(function (n, r): var Tuple(ev, t, custom, add, remove) = r.val;
						  var tmeth = tfun(::(t, []), basic.tvoid);
				switch ((new Tuple(add, remove))) {
			case (None, _): gen.gcon.error( ^ ("Missing event method add_", n), ev.cf_pos);
					failwith("Build failed");
				case (_, None): gen.gcon.error( ^ ("Missing event method remove_", n), ev.cf_pos);
					failwith("Build failed");
				case (Some(add), Some(remove)): 	function check(cf) return {
						try {
							type_eq(EqStrict, cf.cf_type, tmeth);
						} catch (e: Unify_error(el)) {
							List.iter(function e: gen.gcon.error(Typecore.unify_error_msg(print_context([]), e), cf.cf_pos), el);
							failwith("Build failed");
						};
					};
					check(add);
					check(remove);
					if ( && (custom, !(cl.cl_interface))) {
						nonprops.val = ::(add, ::(remove, nonprops.val));
					} else {
						[];
					};
				}, events);
				var evts = List.map(function (_, v): v.val, events);
				var ret = List.map(function (_, v): v.val, props.val);
			var ret = List.filter(function case (_, _, None, None): False;
									  case _: True, ret);
				(new Tuple(evts, ret, List.rev(nonprops.val)));
			};
			var Tuple(fevents, fprops, fnonprops) = partition(cl, cl.cl_ordered_fields);
			var Tuple(sevents, sprops, snonprops) = partition(cl, cl.cl_ordered_statics);
			if (is_some(cl.cl_constructor)) {
				gen_class_field(w, False, cl, is_final, get(cl.cl_constructor));
			} else {
				[];
			};
			if (!(cl.cl_interface)) {
				List.iter(gen_event(w, True, cl), sevents);
				if (switch (cl.cl_kind) {
				case KAbstractImpl(_): False;
					case _: True;
					}) {
					List.iter(gen_prop(w, True, cl, is_final), sprops);
				} else {
					[];
				};
				List.iter(gen_class_field(w, True, cl, is_final), snonprops);
			} else {
				[];
			};
			List.iter(gen_event(w, False, cl), fevents);
			List.iter(gen_prop(w, False, cl, is_final), fprops);
			List.iter(gen_class_field(w, False, cl, is_final), fnonprops);
			check_special_behaviors(w, cl);
			end_block(w);
			if ( && (cl.cl_interface, <>(cl.cl_ordered_statics, []))) {
				print(w, "public class %s__Statics_", snd(cl.cl_path));
				begin_block(w);
				List.iter(gen_class_field(w, True, { (cl) with cl_interface = False }, is_final), cl.cl_ordered_statics);
				end_block(w);
			} else {
				[];
			};
			if (should_close) {
				end_block(w);
			} else {
				[];
			};
		};
		function gen_enum(w, e) return {
			var should_close = switch (change_ns(TEnumDecl(e), fst(e.e_path))) {
			case []: False;
			case ns: print(w, "namespace %s", String.concat(".", ns));
				begin_block(w);
				True;
			};
			gen_attributes(w, e.e_meta);
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
			var file_start = = (len(w), 0);
			var requires_root = && (no_root, file_start);
			if (file_start) {
				Codegen.map_source_header(gen.gcon, function s: print(w, "// %s\n", s));
			} else {
				[];
			};
			reset_temps([]);
			switch (md_tp) {
			case TClassDecl(cl): if (!(cl.cl_extern)) {
					{
						if (requires_root) {
							write(w, "using haxe.root;\n");
						} else {
							[];
						};
						newline(w);
					};
					gen_class(w, cl);
					newline(w);
					newline(w);
				} else {
					[];
				};
				!(cl.cl_extern);
			case TEnumDecl(e): if ( && (!(e.e_extern), !(Meta.has(Meta.Class, e.e_meta)))) {
					{
						if (requires_root) {
							write(w, "using haxe.root;\n");
						} else {
							[];
						};
						newline(w);
					};
					gen_enum(w, e);
					newline(w);
					newline(w);
				} else {
					[];
				};
				!(e.e_extern);
			case TAbstractDecl(_) | TTypeDecl(_): False;
			};
		};
		function module_gen(w, md_def) return {
			List.fold_left(function should: function md: || (module_type_gen(w, md), should), False, md_def.m_types);
		};
		init_ctx(gen);
		Hashtbl.add(gen.gspecial_vars, "__rethrow__", True);
		Hashtbl.add(gen.gspecial_vars, "__typeof__", True);
		Hashtbl.add(gen.gspecial_vars, "__label__", True);
		Hashtbl.add(gen.gspecial_vars, "__goto__", True);
		Hashtbl.add(gen.gspecial_vars, "__is__", True);
		Hashtbl.add(gen.gspecial_vars, "__as__", True);
		Hashtbl.add(gen.gspecial_vars, "__cs__", True);
		Hashtbl.add(gen.gspecial_vars, "__checked__", True);
		Hashtbl.add(gen.gspecial_vars, "__lock__", True);
		Hashtbl.add(gen.gspecial_vars, "__fixed__", True);
		Hashtbl.add(gen.gspecial_vars, "__unsafe__", True);
		Hashtbl.add(gen.gspecial_vars, "__addressOf__", True);
		Hashtbl.add(gen.gspecial_vars, "__valueOf__", True);
		Hashtbl.add(gen.gspecial_vars, "__sizeof__", True);
		Hashtbl.add(gen.gspecial_vars, "__stackalloc__", True);
		Hashtbl.add(gen.gspecial_vars, "__delegate__", True);
		Hashtbl.add(gen.gspecial_vars, "__array__", True);
		Hashtbl.add(gen.gspecial_vars, "__ptr__", True);
		Hashtbl.add(gen.gsupported_conversions, (new Tuple(::("haxe", ::("lang", [])), "Null")), function t1: function t2: True);
		var last_needs_box = gen.gneeds_box;
		gen.gneeds_box = function t:
		switch (gen.greal_type(t)) {
		case TAbstract({ a_path = (::(cs, []), Pointer) }, _) | TInst({ cl_path = (::(cs, []), Pointer) }, _) | TInst({ cl_path = (::(haxe, ::(lang, [])), Null) }, _)
				: True;
		case _: last_needs_box(t);
		};
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
								cf.cf_expr = Option.map(type_map, cf.cf_expr);
			switch (cf.cf_kind) {
		case Var(_) if (&&(Meta.has(Meta.Event, cf.cf_meta), !(Meta.has(Meta.SkipReflection, cf.cf_meta)))): cf.cf_meta = ::((
							new Tuple(Meta.SkipReflection, [], null_pos)), cf.cf_meta);
			case _: [];
			}, all_fields);
		cl.cl_dynamic = Option.map(run_follow_gen, cl.cl_dynamic);
						cl.cl_array_access = Option.map(run_follow_gen, cl.cl_array_access);
						cl.cl_init = Option.map(type_map, cl.cl_init);
						cl.cl_super = Option.map(super_map, cl.cl_super);
						cl.cl_implements = List.map(super_map, cl.cl_implements);
					case _: [], gen.gtypes_list);
		var closure_t = ClosuresToClass.DoubleAndDynamicClosureImpl.get_ctx(gen, 6);
		var tp_v = alloc_var("$type_param", t_dynamic);
		function mk_tp(t, pos) return {
			{
				() with eexpr = TLocal(tp_v);
				etype = t;
				epos = pos
			};
		};
		TypeParams.configure(gen, function ecall: function efield: function params: function elist:
		switch (efield.eexpr) {
	case TField(_, FEnum(_)): {
			(ecall) with eexpr = TCall(efield, elist)
		};
		case _: {
			(ecall) with eexpr = TCall(efield, @(List.map(function t: mk_tp(t, ecall.epos), params), elist))
		};
		});
		if (!(erase_generics)) {
			HardNullableSynf.configure(gen, HardNullableSynf.traverse(gen, function e:
			switch ((new Tuple(e.eexpr, real_type(e.etype)))) {
		case (TConst(TThis), _) if (=(gen.gcurrent_path, (new Tuple(::("haxe", ::("lang", [])), "Null")))): e;
			case (_, TInst({ cl_path = (::(haxe, ::(lang, [])), Null) }, ::(t, []))): var e = { (e) with eexpr = TParenthesis(e) };
				{
					(mk_field_access(gen, e, "value", e.epos)) with etype = t
				};
			case _: trace(debug_type(e.etype));
				gen.gcon.error("This expression is not a Nullable expression", e.epos);
				assert False;
			}, function v: function t: function has_value:
			switch ((new Tuple(has_value, real_type(v.etype)))) {
		case (True, TDynamic(_)) | (True, TAnon(_)) | (True, TMono(_)): {
				() with eexpr = TCall(mk_static_field_access_infer(null_t, "ofDynamic", v.epos, ::(t, [])), ::(mk_tp(t, v.epos),
									  ::(v, [])));
				etype = TInst(null_t, ::(t, []));
				epos = v.epos
			};
			case _: {
				() with eexpr = TNew(null_t, ::(t, []), ::(gen.ghandle_cast(t, v.etype, v), ::({ () with eexpr = TConst(TBool(has_value));
									 etype = gen.gcon.basic.tbool;
									 epos = v.epos
																							   }, [])));
				etype = TInst(null_t, ::(t, []));
				epos = v.epos
			};
			}, function e: {
				() with eexpr = TCall({ (mk_field_access(gen, { (mk_paren(e)) with etype = real_type(e.etype) }, "toDynamic", e.epos)) with etype = TFun([], t_dynamic) }, []);
				etype = t_dynamic;
				epos = e.epos
			}, function e: mk_field_access(gen, { (e) with etype = real_type(e.etype) }, "hasValue", e.epos),
			function e1: function e2: {
				() with eexpr = TCall(mk_field_access(gen, e1, "Equals", e1.epos), ::(e2, []));
				etype = basic.tbool;
				epos = e1.epos
			}, True, False));
		} else {
			[];
		};
		function explicit_fn_name(c, tl, fname) return {
			^ (path_param_s(TClassDecl(c), c.cl_path, tl), ^ (".", fname));
		};
		FixOverrides.configure(explicit_fn_name = explicit_fn_name, get_vmtype = real_type, gen);
		Normalize.configure(gen, metas = Hashtbl.create(0));
		AbstractImplementationFix.configure(gen);
		IteratorsInterface.configure(gen, function e: e);
		OverrideFix.configure(gen);
		ClosuresToClass.configure(gen, ClosuresToClass.default_implementation(closure_t, get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "Function"))))));
		var enum_base = get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "Enum"))));
		var param_enum_base = get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "ParamEnum"))));
		EnumToClass.configure(gen, Some(function e: mk_cast(gen.gcon.basic.tint, e)), True, True, enum_base, param_enum_base, False, False);
		InterfaceVarsDeleteModf.configure(gen);
		InterfaceProps.configure(gen);
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
		var rcf_static_lookup = mk_static_field_access_infer(get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "FieldLookup")))), "lookupHash", Ast.null_pos, []);
		var Tuple(rcf_static_insert, rcf_static_remove) = if (erase_generics) {
			function get_specialized_postfix(t) return {
				switch (t) {
				case TAbstract({ a_path = ([], Float | Int = name) }, _): name;
				case TAnon(_) | TDynamic(_): "Dynamic";
				case _: print_endline(debug_type(t));
					assert False;
				};
			};
			(new Tuple(function t: mk_static_field_access_infer(get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])),
					   "FieldLookup")))), ^ ("insert", get_specialized_postfix(t)), Ast.null_pos, []),
					   function t: mk_static_field_access_infer(get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "FieldLookup")))),
							   ^ ("remove", get_specialized_postfix(t)), Ast.null_pos, [])));
		} else {
			(new Tuple(function t: mk_static_field_access_infer(get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "FieldLookup")))), "insert", Ast.null_pos, ::(t, [])), function t: mk_static_field_access_infer(get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "FieldLookup")))), "remove", Ast.null_pos, ::(t, []))));
		};
		var can_be_float = like_float;
		function rcf_on_getset_field(main_expr, field_expr, field, may_hash, may_set, is_unsafe) return {
			var is_float = can_be_float(real_type(main_expr.etype));
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
			mk_cast(ecall.etype, { (ecall) with eexpr = TCall(infer, call_args) });
		};
		if (!(erase_generics)) {
			handle_type_params(gen, ifaces, get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "IGenericObject")))));
		} else {
			add_cast_handler(gen);
			TypeParams.RealTypeParams.RealTypeParamsModf.configure(gen, TypeParams.RealTypeParams.RealTypeParamsModf.set_only_hxgeneric(gen));
		};
		var rcf_ctx = ReflectionCFs.new_ctx(gen, closure_t, object_iface, True, rcf_on_getset_field, rcf_on_call_field, function hash: function hash_array: function length: { (hash) with eexpr = TCall(rcf_static_find, ::(hash, ::(hash_array, ::(length, []))));
											etype = basic.tint
																																											 }, function hash: { (hash) with eexpr = TCall(rcf_static_lookup, ::(hash, []));
																																													 etype = gen.gcon.basic.tstring
		}, function hash_array: function length: function pos: function value: var ecall = mk(TCall(rcf_static_insert(value.etype), ::(hash_array, ::(length, ::(pos, ::(value, []))))), if (erase_generics) {
		hash_array.etype;
	} else {
		basic.tvoid;
	}, hash_array.epos);
	if (erase_generics) {
		{
			(ecall) with eexpr = TBinop(OpAssign, hash_array, ecall)
			};
		} else {
			ecall;
		}, function hash_array: function length: function pos: var t = gen.gclasses.nativearray_type(hash_array.etype);
		{
			(hash_array) with eexpr = TCall(rcf_static_remove(t), ::(hash_array, ::(length, ::(pos, []))));
			etype = gen.gcon.basic.tvoid
		}, False);
		ReflectionCFs.UniversalBaseClass.default_config(gen, get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "HxObject")))), object_iface, dynamic_object);
		ReflectionCFs.configure_dynamic_field_access(rcf_ctx, False);
		var closure_cl = get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "Closure"))));
		var varargs_cl = get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "VarArgsFunction"))));
		var dynamic_name = gen.gmk_internal_name("hx", "invokeDynamic");
		List.iter(function cl: List.iter(function cf:
		if ( = (cf.cf_name, dynamic_name)) {
		cl.cl_overrides = ::(cf, cl.cl_overrides);
		} else {
			[];
		}, cl.cl_ordered_fields), ::(closure_cl, ::(varargs_cl, [])));
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
		TArrayTransform.configure(gen, TArrayTransform.default_implementation(gen, function e: function binop:
		switch (e.eexpr) {
	case TArray(e1, e2): switch (follow(e1.etype)) {
			case TDynamic(_) | TAnon(_) | TMono(_): True;
			case TInst({ cl_kind = KTypeParameter(_) }, _): True;
			case TInst(c, p) if (&&(erase_generics, &&(is_hxgeneric(TClassDecl(c)), is_hxgen(TClassDecl(c))))):
				switch (c.cl_path) {
				case ([], String) | (::(cs, []), NativeArray): False;
				case _: True;
				};
			case _: switch ((new Tuple(binop, change_param_type(t_to_md(e1.etype), ::(e.etype, []))))) {
				case (Some(Ast.OpAssignOp(_)), ::(TDynamic(_), []) | ::(TAnon(_), [])): True;
				case _: False;
				};
			};
		case _: assert False;
		}, "__get", "__set"));
		function field_is_dynamic(t, field) return {
			switch (field_access_esp(gen, gen.greal_type(t), field)) {
			case FEnumField(_): False;
			case FClassField(cl, p, _, _, _, t, _): if (!(erase_generics)) {
					False;
				} else {
					var p = change_param_type(TClassDecl(cl), p);
					is_dynamic(apply_params(cl.cl_params, p, t));
				};
			case _: True;
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
				case TAbstract(_) if (like_float(t)): Some(t);
				case t if (is_cs_basic_type(t)): Some(t);
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
		function is_null(t) return {
			switch (real_type(t)) {
			case TInst({ cl_path = (::(haxe, ::(lang, [])), Null) }, _): True;
			case _: False;
			};
		};
		function is_null_expr(e) return {
			|| (is_null(e.etype), switch (e.eexpr) {
		case TField(tf, f): switch (field_access_esp(gen, real_type(tf.etype), f)) {
				case FClassField(_, _, _, _, _, actual_t, _): is_null(actual_t);
				case _: False;
				};
			case _: False;
			});
		};
		function should_handle_opeq(t) return {
			switch (real_type(t)) {
			case TDynamic(_) | TAnon(_) | TMono(_) | TInst({ cl_kind = KTypeParameter(_) }, _) | TInst({ cl_path = (::(haxe, ::(lang, [])), Null) }, _)
				: True;
			case _: False;
			};
		};
		var string_cl = switch (gen.gcon.basic.tstring) {
		case TInst(c, []): c;
		case _: assert False;
		};
		function is_undefined(e) return {
			switch (e.eexpr) {
			case TLocal({ v_name = __undefined__ }) | TField(_, FStatic({ cl_path = (::(haxe, ::(lang, [])), Runtime) }, { cf_name = undefined }))
				: True;
			case _: False;
			};
		};
		DynamicOperators.configure(gen, DynamicOperators.abstract_implementation(gen, function e:
		switch (e.eexpr) {
	case TBinop(Ast.OpEq, e1, e2) | TBinop(Ast.OpNotEq, e1, e2): switch ((new Tuple(e1.eexpr, e2.eexpr))) {
			case (TConst(TNull), _) if (||(&&(!(is_tparam(e2.etype)), is_dynamic(e2.etype)), is_null_expr(e2))): False;
			case (_, TConst(TNull)) if (||(&&(!(is_tparam(e1.etype)), is_dynamic(e1.etype)), is_null_expr(e1))): False;
			case _ if (||(is_undefined(e1), is_undefined(e2))): False;
			case _: || (should_handle_opeq(e1.etype), should_handle_opeq(e2.etype));
			};
		case TBinop(Ast.OpAssignOp(Ast.OpAdd), e1, e2): || (is_dynamic_expr(e1), || (is_null_expr(e1), is_string(e.etype)));
		case TBinop(Ast.OpAdd, e1, e2): || (is_dynamic(e1.etype), || (is_dynamic(e2.etype), || (is_tparam(e1.etype),
												|| (is_tparam(e2.etype), || (is_string(e1.etype), || (is_string(e2.etype), is_string(e.etype)))))));
		case TBinop(Ast.OpLt, e1, e2) | TBinop(Ast.OpLte, e1, e2) | TBinop(Ast.OpGte, e1, e2) | TBinop(Ast.OpGt, e1, e2):
			|| (is_dynamic(e.etype), || (is_dynamic_expr(e1), || (is_dynamic_expr(e2),
										 || (is_string(e1.etype), is_string(e2.etype)))));
		case TBinop(_, e1, e2): || (is_dynamic(e.etype), || (is_dynamic_expr(e1), is_dynamic_expr(e2)));
		case TUnop(_, _, e1): || (is_dynamic_expr(e1), is_null_expr(e1));
		case _: False;
		}, function e1: function e2: var is_basic = || (is_cs_basic_type(follow(e1.etype)), is_cs_basic_type(follow(e2.etype)));
		var is_ref = if (is_basic) {
		False;
	} else {
		switch ((new Tuple(follow(e1.etype), follow(e2.etype)))) {
			case (TDynamic(_), _) | (_, TDynamic(_)) | (TInst({ cl_path = ([], String) }, []), _) | (_, TInst({ cl_path = ([], String) }, [])) | (TInst({ cl_kind = KTypeParameter(_) }, []), _) | (_, TInst({ cl_kind = KTypeParameter(_) }, []))
				: False;
			case (_, _): True;
			};
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
		case _ if (||(is_string(e.etype), ||(is_string(e1.etype), is_string(e2.etype)))): {
			() with eexpr = TCall(mk_static_field_access_infer(runtime_cl, "concat", e.epos, []), ::(e1, ::(e2, [])));
			etype = basic.tstring;
			epos = e.epos
		};
		case _: var static = mk_static_field_access_infer(runtime_cl, "plus", e1.epos, []);
			mk_cast(e.etype, { () with eexpr = TCall(static, ::(e1, ::(e2, [])));
							   etype = t_dynamic;
							   epos = e1.epos
							 });
		}, function e1: function e2:
		if (is_string(e1.etype)) {
		{
			(e1) with eexpr = TCall(mk_static_field_access_infer(string_cl, "Compare", e1.epos, []), ::(e1, ::(e2, [])));
				etype = gen.gcon.basic.tint
			};
		} else {
			var static = mk_static_field_access_infer(runtime_cl, "compare", e1.epos, []);
			{
				() with eexpr = TCall(static, ::(e1, ::(e2, [])));
				etype = gen.gcon.basic.tint;
				epos = e1.epos
			};
		}, handle_strings = False));
		FilterClosures.configure(gen, FilterClosures.traverse(gen, function e1: function s: True, closure_func));
		var base_exception = get_cl(get_type(gen, (new Tuple(::("System", []), "Exception"))));
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
		TryCatchWrapper.configure(gen, TryCatchWrapper.traverse(gen, function t: !(is_exception(real_type(t))), function throwexpr: function expr: var wrap_static = mk_static_field_access(hx_exception, "wrap", TFun(::((new Tuple("obj", False, t_dynamic)), []), base_exception_t), expr.epos);
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
		mk_field_access(gen, local, "obj", pos), function rethrow: {
			(rethrow) with eexpr = TCall(mk_local(alloc_var("__rethrow__", t_dynamic), rethrow.epos), ::(rethrow, []));
			etype = gen.gcon.basic.tvoid
		}, base_exception_t, hx_exception_t, function v: function e: var exc_cl = get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "Exceptions"))));
		var exc_field = mk_static_field_access_infer(exc_cl, "exception", e.epos, []);
		var esetstack = mk(TBinop(Ast.OpAssign, exc_field, mk_local(v, e.epos)), v.v_type, e.epos);
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
		CastDetect.configure(gen, CastDetect.default_implementation(gen, Some(TEnum(empty_e, [])), !(erase_generics), native_string_cast = False, overloads_cast_to_base = True));
		SwitchToIf.configure(gen, SwitchToIf.traverse(gen, function e:
		switch (e.eexpr) {
	case TSwitch(cond, cases, def): switch (gen.gfollowrun_f(cond.etype)) {
			case TAbstract({ a_path = ([], Int) }, []) | TInst({ cl_path = ([], String) }, []): List.exists(function (c,
						_): List.exists(function expr:
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
		UnreachableCodeEliminationSynf.configure(gen, UnreachableCodeEliminationSynf.traverse(gen, False, True, True, False));
		ArrayDeclSynf.configure(gen, ArrayDeclSynf.default_implementation(gen, native_arr_cl));
		var goto_special = alloc_var("__goto__", t_dynamic);
		var label_special = alloc_var("__label__", t_dynamic);
		SwitchBreakSynf.configure(gen, SwitchBreakSynf.traverse(gen, function e_loop: function n: function api: api({ () with eexpr = TCall(mk_local(label_special, e_loop.epos), ::(mk_int(gen, n, e_loop.epos), []));
								  etype = t_dynamic;
								  epos = e_loop.epos
																													}, False);
								  e_loop, function e_break: function n: function api: { () with eexpr = TCall(mk_local(goto_special, e_break.epos), ::(mk_int(gen, n, e_break.epos), []));
										  etype = t_dynamic;
										  epos = e_break.epos
																					  }));
		DefaultArguments.configure(gen, DefaultArguments.traverse(gen));
		InterfaceMetas.configure(gen);
		CSharpSpecificSynf.configure(gen, CSharpSpecificSynf.traverse(gen, runtime_cl));
		CSharpSpecificESynf.configure(gen, CSharpSpecificESynf.traverse(gen, runtime_cl));
		var out_files = ref([]);
		if ( > (Hashtbl.length(gen.gcon.resources), 0)) {
			var src = ^ (gen.gcon.file, "/src/Resources");
			Hashtbl.iter(function name: function v: var name = Codegen.escape_res_name(name, True);
						 var full_path = ^ (src, ^ ("/", name));
						 mkdir_from_path(full_path);
						 var f = open_out_bin(full_path);
						 output_string(f, v);
						 close_out(f);
						 out_files.val = ::(unique_full_path(full_path), out_files.val), gen.gcon.resources);
		} else {
			[];
		};
		try {
			var res = get_cl(Hashtbl.find(gen.gtypes, (new Tuple(::("haxe", []), "Resource"))));
			var cf = PMap.find("content", res.cl_statics);
			var res = ref([]);
			Hashtbl.iter(function name: function v: res.val = ::({
				() with eexpr = TConst(TString(name));
				etype = gen.gcon.basic.tstring;
				epos = Ast.null_pos
			}, res.val), gen.gcon.resources);
			cf.cf_expr = Some({
				() with eexpr = TArrayDecl(res.val);
				etype = gen.gcon.basic.tarray(gen.gcon.basic.tstring);
				epos = Ast.null_pos
			});
		} catch (e: Not_found) {
			[];
		};
		run_filters(gen);
		function normalize_i(i) return {
			var i = Int32.of_int(i);
			if ( < (i, Int32.zero)) {
				Int32.logor(Int32.logand(i, Int32.of_int(0x3FFFFFFF)), Int32.shift_left(Int32.one, 30));
			} else {
				i;
			};
		};
		var nhash = ref(0);
		var hashes = Hashtbl.fold(function i: function s: function acc: incr(nhash);
								  ::((new Tuple(normalize_i(i), s)), acc), rcf_ctx.rcf_hash_fields, []);
		var hashes = List.sort(function (i, s): function (i2, s2): compare(i, i2), hashes);
		var flookup_cl = get_cl(get_type(gen, (new Tuple(::("haxe", ::("lang", [])), "FieldLookup"))));
		var haxe_libs = List.filter(function (_, _, _, lookup): is_some(lookup((new Tuple(::("haxe", ::("lang", [])), "DceNo")))), gen.gcon.net_libs);
		try {
			var Tuple(name, _, _, _) = List.find(function (_, _, _, lookup): is_some(lookup((new Tuple(::("haxe", ::("lang", [])), "FieldLookup")))), gen.gcon.net_libs);
			if (!(Common.defined(gen.gcon, Define.DllImport))) {
				gen.gcon.warning( ^ ("The -net-lib with path ", ^ (name,
				" contains a Haxe-generated assembly. Please define `-D dll_import` to handle Haxe-generated dll import correctly")),
				null_pos);
				raise(Not_found);
			} else {
				[];
			};
			if (!(List.exists(function (n, _, _, _): = (n, name), haxe_libs))) {
				gen.gcon.warning( ^ ("The -net-lib with path ", ^ (name,
									 " contains a Haxe-generated assembly, however it wasn't compiled with `-dce no`. Recompilation with `-dce no` is recommended")),
								  null_pos);
			} else {
				[];
			};
			flookup_cl.cl_extern = True;
			var hashs_by_path = Hashtbl.create(nhash.val);
			Hashtbl.iter(function (path, i): function s: Hashtbl.add(hashs_by_path, path, (new Tuple(i, s))), rcf_ctx.rcf_hash_paths);
			Hashtbl.iter(function _: function md:
			switch (md) {
		case TClassDecl({ cl_extern = False; cl_interface = False } = c):
				try {
					var all = Hashtbl.find_all(hashs_by_path, c.cl_path);
					var all = List.map(function (i, s): (new Tuple(normalize_i(i), s)), all);
					var all = List.sort(function (i, s): function (i2, s2): compare(i, i2), all);
					if (<>(all, [])) {
						var add = mk_static_field_access_infer(flookup_cl, "addFields", c.cl_pos, []);
						var expr = { () with eexpr = TCall(add, ::(mk_nativearray_decl(gen, basic.tint, List.map(function (i, s): { () with eexpr = TConst(TInt(i));
														   etype = basic.tint;
														   epos = c.cl_pos
																																  }, all), c.cl_pos), ::(mk_nativearray_decl(gen, basic.tstring, List.map(function (i, s): { () with eexpr = TConst(TString(s));
																																		  etype = basic.tstring;
																																		  epos = c.cl_pos
																																																						   }, all), c.cl_pos), [])));
									 etype = basic.tvoid;
									 epos = c.cl_pos
								   };
						switch (c.cl_init) {
						case None: c.cl_init = Some(expr);
						case Some(e): c.cl_init = Some({ () with eexpr = TBlock(::(expr, ::(e, [])));
															 etype = basic.tvoid;
															 epos = e.epos
														   });
						};
					} else {
						[];
					};
				} catch (e: Not_found) {
					[];
				};
			case _: [];
			}, gen.gtypes);
		} catch (e: Not_found) {
			try {
				var basic = gen.gcon.basic;
				var cl = flookup_cl;
				var field_ids = PMap.find("fieldIds", cl.cl_statics);
				var fields = PMap.find("fields", cl.cl_statics);
				field_ids.cf_expr = Some(mk_nativearray_decl(gen, basic.tint, List.map(function (i, s): {
											 () with eexpr = TConst(TInt(i));
											 etype = basic.tint;
											 epos = field_ids.cf_pos
										 }, hashes), field_ids.cf_pos));
				fields.cf_expr = Some(mk_nativearray_decl(gen, basic.tstring, List.map(function (i, s): {
										  () with eexpr = TConst(TString(s));
										  etype = basic.tstring;
										  epos = fields.cf_pos
									  }, hashes), fields.cf_pos));
			} catch (e: Not_found) {
				gen.gcon.error("Fields 'fieldIds' and 'fields' were not found in class haxe.lang.FieldLookup", flookup_cl.cl_pos);
			};
		};
		if (Common.defined(gen.gcon, Define.DllImport)) {
			Hashtbl.iter(function _: function md:
			switch (md) {
		case TClassDecl({ cl_extern = False } = c):
				try {
					var extra = switch (c.cl_params) {
					case ::(_, _) if (!(erase_generics)): ^ ("_", string_of_int(List.length(c.cl_params)));
					case _: "";
					};
					var pack = switch (c.cl_path) {
					case ([], _) if (&&(no_root, is_hxgen(TClassDecl(c)))): ::("haxe", ::("root", []));
					case (p, _): p;
					};
					var path = (new Tuple(pack, ^ (snd(c.cl_path), extra)));
					ignore(List.find(function (_, _, _, lookup): is_some(lookup(path)), haxe_libs));
					c.cl_extern = True;
				} catch (e: Not_found) {
					[];
				};
			case _: [];
			}, gen.gtypes);
		} else {
			[];
		};
		TypeParams.RenameTypeParameters.run(gen);
		var parts = Str.split_delim(Str.regexp("[\\/]+"), gen.gcon.file);
		mkdir_recursive("", parts);
		generate_modules(gen, "cs", "src", module_gen, out_files);
		if (!(Common.defined(gen.gcon, Define.KeepOldOutput))) {
			clean_files( ^ (gen.gcon.file, "/src"), out_files.val, gen.gcon.verbose);
		} else {
			[];
		};
		dump_descriptor(gen, "hxcs_build.txt", path_s, module_s);
		if (!(Common.defined(gen.gcon, Define.NoCompilation))) {
			var old_dir = Sys.getcwd([]);
			Sys.chdir(gen.gcon.file);
			var cmd = ^ ("haxelib run hxcs hxcs_build.txt --haxe-version ", ^ (string_of_int(gen.gcon.version), " --feature-level 1"));
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

	public static function generate(con) return {
		try {
			var gen = new_ctx(con);
			var basic = con.basic;
			if ( = (Common.defined_value(con, Define.Dce), "no")) {
				var m = {
					(null_module) with m_id = alloc_mid([]);
					m_path = (new Tuple(::("haxe", ::("lang", [])), "DceNo"))
				};
				var cl = mk_class(m, (new Tuple(::("haxe", ::("lang", [])), "DceNo")), null_pos);
				gen.gtypes_list = ::(TClassDecl(cl), gen.gtypes_list);
				Hashtbl.add(gen.gtypes, cl.cl_path, TClassDecl(cl));
			} else {
				[];
			};
			var type_cl = get_cl(get_type(gen, (new Tuple(::("System", []), "Type"))));
			var basic_fns = ::(mk_class_field("Equals", TFun(::((new Tuple("obj", False, t_dynamic)), []), basic.tbool), True, Ast.null_pos, Method(MethNormal), []), ::(mk_class_field("ToString", TFun([], basic.tstring), True, Ast.null_pos, Method(MethNormal), []), ::(mk_class_field("GetHashCode", TFun([], basic.tint), True, Ast.null_pos, Method(MethNormal), []), ::(mk_class_field("GetType", TFun([], TInst(type_cl, [])), True, Ast.null_pos, Method(MethNormal), []), []))));
			List.iter(function cf: gen.gbase_class_fields = PMap.add(cf.cf_name, cf, gen.gbase_class_fields), basic_fns);
			configure(gen);
		} catch (e: TypeNotFound(path)) {
			con.error( ^ ("Error. Module '", ^ (path_s(path), "' is required and was not included in build.")), Ast.null_pos);
		};
		debug_mode.val = False;
	};

	public static function is_haxe_keyword(match) return switch (match) {
	case callback | cast | extern | function | in | typedef | using | var | untyped | inline: True;
	case _: False;
	};

	public static function hxpath_to_net(ctx, path) return {
		try {
			Hashtbl.find(ctx.ncom.net_path_map, path);
		} catch (e: Not_found) {
			(new Tuple([], [], "Not_found"));
		};
	};

	public static function add_cs(match) return switch (match) {
	case ::(haxe, ns): ::("haxe", ns);
	case ::(std, ns): ::("std", ns);
	case ::(cs, ns): ::("cs", ns);
	case ::(system, ns): ::("cs", ::("system", ns));
	case ns: ns;
	};

	public static var escape_chars = String.replace_chars(function chr:
									 if ( || ( && ( >= (chr, 'a'), <= (chr, 'z')), || ( && ( >= (chr, 'A'), <= (chr, 'Z')), || (
	&& ( >= (chr, '0'), <= (chr, '9')), = (chr, '_'))))) {
	Char.escaped(chr);
	} else {
		^ ("_x", ^ (string_of_int(Char.code(chr)), "_"));
	});

	public static function netcl_to_hx(cl) return {
		var cl = if ( && ( > (String.length(cl), 0), && ( >= (String.get(cl, 0), 'a'), <= (String.get(cl, 0), 'z')))) {
			^ (Char.escaped(Char.uppercase(String.get(cl, 0))), String.sub(cl, 1, -(String.length(cl), 1)));
		} else {
			cl;
		};
		try {
			var Tuple(cl, nargs) = String.split(cl, "`");
			^ (escape_chars(cl), ^ ("_", nargs));
		} catch (e: Invalid_string) {
			escape_chars(cl);
		};
	};

	public static function netpath_to_hx(std) return {
	case ([], [], cl): (new Tuple([], netcl_to_hx(cl)));
	case (ns, [], cl): var ns = List.map(function s: String.lowercase(escape_chars(s)), ns);
		(new Tuple(add_cs(ns), netcl_to_hx(cl)));
	case (ns, ::(nhd, ntl) = nested, cl): var nested = List.map(netcl_to_hx, nested);
		var ns = @(List.map(function s: String.lowercase(escape_chars(s)), ns), ::(nhd, []));
		(new Tuple(add_cs(ns), ^ (String.concat("_", nested), ^ ("_", netcl_to_hx(cl)))));
	};

	public static function lookup_ilclass(std, com, ilpath) return {
		var path = netpath_to_hx(std, ilpath);
		List.fold_right(function (_, _, _, get_raw_class): function acc:
		switch (acc) {
	case None: get_raw_class(path);
		case Some(p): acc;
		}, com.net_libs, None);
	};

	public static function discard_nested(Tuple(Tuple(ns, _), cl)) return {
		(new Tuple((new Tuple(ns, [])), cl));
	};

	public static function mk_type_path(ctx, path, params) return {
		var Tuple(pack, sub, name) = switch (path) {
		case (ns, [], cl): (new Tuple(ns, None, netcl_to_hx(cl)));
		case (ns, ::(nhd, ntl) = nested, cl): var nhd = netcl_to_hx(nhd);
			var nested = List.map(netcl_to_hx, nested);
			(new Tuple(ns, Some( ^ (String.concat("_", nested), ^ ("_", netcl_to_hx(cl)))), nhd));
		};
		CTPath({ () with tpackage = fst(netpath_to_hx(ctx.nstd, (new Tuple(pack, [], ""))));
				 Ast.tname = name;
				 tparams = params;
				 tsub = sub
			   });
	};

	public static function raw_type_path(ctx, path, params) return {
		{
			() with tpackage = fst(path);
			Ast.tname = snd(path);
			tparams = params;
			tsub = None
		};
	};

	public static function convert_signature(ctx, p) return {
	case LVoid: mk_type_path(ctx, (new Tuple([], [], "Void")), []);
	case LBool: mk_type_path(ctx, (new Tuple([], [], "Bool")), []);
	case LChar: mk_type_path(ctx, (new Tuple(::("cs", ::("types", [])), [], "Char16")), []);
	case LInt8: mk_type_path(ctx, (new Tuple(::("cs", ::("types", [])), [], "Int8")), []);
	case LUInt8: mk_type_path(ctx, (new Tuple(::("cs", ::("types", [])), [], "UInt8")), []);
	case LInt16: mk_type_path(ctx, (new Tuple(::("cs", ::("types", [])), [], "Int16")), []);
	case LUInt16: mk_type_path(ctx, (new Tuple(::("cs", ::("types", [])), [], "UInt16")), []);
	case LInt32: mk_type_path(ctx, (new Tuple([], [], "Int")), []);
	case LUInt32: mk_type_path(ctx, (new Tuple([], [], "UInt")), []);
	case LInt64: mk_type_path(ctx, (new Tuple(::("haxe", []), [], "Int64")), []);
	case LUInt64: mk_type_path(ctx, (new Tuple(::("cs", ::("types", [])), [], "UInt64")), []);
	case LFloat32: mk_type_path(ctx, (new Tuple([], [], "Single")), []);
	case LFloat64: mk_type_path(ctx, (new Tuple([], [], "Float")), []);
	case LString: mk_type_path(ctx, (new Tuple(::("std", []), [], "String")), []);
	case LObject: mk_type_path(ctx, (new Tuple([], [], "Dynamic")), []);
	case LPointer(s) | LManagedPointer(s): mk_type_path(ctx, (new Tuple(::("cs", []), [], "Pointer")), ::(TPType(convert_signature(ctx, p, s)), []));
	case LTypedReference: mk_type_path(ctx, (new Tuple(::("cs", ::("system", [])), [], "TypedReference")), []);
	case LIntPtr: mk_type_path(ctx, (new Tuple(::("cs", ::("system", [])), [], "IntPtr")), []);
	case LUIntPtr: mk_type_path(ctx, (new Tuple(::("cs", ::("system", [])), [], "UIntPtr")), []);
	case LValueType(s, args) | LClass(s, args): mk_type_path(ctx, s, List.map(function s: TPType(convert_signature(ctx, p, s)), args));
	case LTypeParam(i): mk_type_path(ctx, (new Tuple([], [], ^ ("T", string_of_int(i)))), []);
	case LMethodTypeParam(i): mk_type_path(ctx, (new Tuple([], [], ^ ("M", string_of_int(i)))), []);
	case LVector(s): mk_type_path(ctx, (new Tuple(::("cs", []), [], "NativeArray")), ::(TPType(convert_signature(ctx, p, s)), []));
	case LMethod(_, ret, args): CTFunction(List.map(convert_signature(ctx, p), args), convert_signature(ctx, p, ret));
	case _: mk_type_path(ctx, (new Tuple([], [], "Dynamic")), []);
	};

	public static function ilpath_s(match) return switch (match) {
	case (ns, [], name): path_s((new Tuple(ns, name)));
	case ([], nested, name): ^ (String.concat(".", nested), ^ (".", name));
	case (ns, nested, name): ^ (String.concat(".", ns), ^ (".", ^ (String.concat(".", nested), ^ (".", name))));
	};

	public static function get_cls(Tuple(_, _, c)) return {
		c;
	};

	public static function enum_is_flag(ilcls) return {
		function check_flag(name, ns) return {
			&& ( = (name, "FlagsAttribute"), = (ns, ::("System", [])));
		};
		List.exists(function a:
		switch (a.ca_type) {
	case TypeRef(r): check_flag(r.tr_name, r.tr_namespace);
		case TypeDef(d): check_flag(d.td_name, d.td_namespace);
		case Method(m): switch (m.m_declaring) {
			case Some(d): check_flag(d.td_name, d.td_namespace);
			case _: False;
			};
		case MemberRef(r): switch (r.memr_class) {
			case TypeRef(r): check_flag(r.tr_name, r.tr_namespace);
			case TypeDef(d): check_flag(d.td_name, d.td_namespace);
			case _: False;
			};
		case _: False;
		}, ilcls.cattrs);
	};

	public static function convert_ilenum(ctx, p, ? : (is_flag = False), ilcls) return {
		var meta = ref(::((new Tuple(Meta.Native, ::((new Tuple(EConst(String(ilpath_s(ilcls.cpath))), p)), []), p)), ::((new Tuple(Meta.CsNative, [], p)), [])));
		var data = ref([]);
		List.iter(function f:
		switch (f.fname) {
	case value__: [];
		case _ if (!(List.mem(CStatic, f.fflags.ff_contract))): [];
		case _: var Tuple(meta, const) = switch (f.fconstant) {
			case Some(IChar(i)) | Some(IByte(i)) | Some(IShort(i)): (new Tuple(::((new Tuple(Meta.CsNative,
				::((new Tuple(EConst(Int(string_of_int(i))), p)), []), p)), []), Int64.of_int(i)));
			case Some(IInt(i)): (new Tuple(::((new Tuple(Meta.CsNative, ::((new Tuple(EConst(Int(Int32.to_string(i))), p)), []),
				p)), []), Int64.of_int32(i)));
			case Some(IFloat32(f)) | Some(IFloat64(f)): (new Tuple([], Int64.of_float(f)));
			case Some(IInt64(i)): (new Tuple([], i));
			case _: (new Tuple([], Int64.zero));
			};
			data.val = ::((new Tuple({ () with ec_name = f.fname;
									   ec_doc = None;
									   ec_meta = meta;
									   ec_args = [];
									   ec_pos = p;
									   ec_params = [];
									   ec_type = None
									 }, const)), data.val);
		}, ilcls.cfields);
		var data = List.stable_sort(function (_, i1): function (_, i2): Int64.compare(i1, i2), List.rev(data.val));
		var Tuple(_, c) = netpath_to_hx(ctx.nstd, ilcls.cpath);
		var name = netname_to_hx(c);
		EEnum({ () with d_name = if (is_flag) {
		^ (name, "_FlagsEnum");
		} else {
			name;
		};
		d_doc = None;
				d_params = [];
				d_meta = meta.val;
				d_flags = ::(EExtern, []);
				d_data = List.map(fst, data)
			  });
	};

	public static function has_unmanaged(match) return switch (match) {
	case LPointer(_): True;
	case LManagedPointer(s): has_unmanaged(s);
	case LValueType(p, pl): List.exists(has_unmanaged, pl);
	case LClass(p, pl): List.exists(has_unmanaged, pl);
	case LVector(s): has_unmanaged(s);
	case LArray(s, a): has_unmanaged(s);
	case LMethod(c, r, args): || (has_unmanaged(r), List.exists(has_unmanaged, args));
	case _: False;
	};

	public static function convert_ilfield(ctx, p, field) return {
		if ( && (!(Common.defined(ctx.ncom, Define.Unsafe)), has_unmanaged(field.fsig.snorm))) {
			raise(Exit);
		} else {
			[];
		};
		var p = { (p) with pfile = ^ (p.pfile, ^ (", [", ^ (field.fname, "]"))) };
		var cff_doc = None;
		var cff_pos = p;
		var cff_meta = ref([]);
		var cff_name = switch (field.fname) {
		case name if (>(String.length(name), 5)):
			switch (String.sub(name, 0, 5)) {
			case __hx_: raise(Exit);
			case _: name;
			};
		case name: name;
		};
		var cff_access = switch (field.fflags.ff_access) {
		case FAFamily | FAFamOrAssem: APrivate;
		case FAPublic: APublic;
		case _: raise(Exit);
		};
	var Tuple(readonly, acc) = List.fold_left(function (readonly, acc): function case CStatic: (new Tuple(readonly, ::(AStatic, acc)));
								   case CInitOnly | CLiteral: (new Tuple(True, acc));
									   case _: (new Tuple(readonly, acc)), (new Tuple(False, ::(cff_access, []))), field.fflags.ff_contract);
		if (PMap.mem("net_loader_debug", ctx.ncom.defines)) {
			Printf.printf("\t%sfield %s : %s\n", if (List.mem(AStatic, acc)) {
			"static ";
		} else {
			"";
		}, cff_name, IlMetaDebug.ilsig_s(field.fsig.ssig));
		} else {
			[];
		};
		var kind = switch (readonly) {
		case True: FProp("default", "never", Some(convert_signature(ctx, p, field.fsig.snorm)), None);
		case False: FVar(Some(convert_signature(ctx, p, field.fsig.snorm)), None);
		};
		var Tuple(cff_name, cff_meta) = if ( = (String.get(cff_name, 0), '%')) {
			var name = String.sub(cff_name, 1, -(String.length(cff_name), 1));
			(new Tuple( ^ ("_", name), ::((new Tuple(Meta.Native, ::((new Tuple(EConst(String(name)), cff_pos)), []), cff_pos)),
										  cff_meta.val)));
		} else {
			(new Tuple(cff_name, cff_meta.val));
		};
		{
			() with cff_name = cff_name;
			cff_doc = cff_doc;
			cff_pos = cff_pos;
			cff_meta = cff_meta;
			cff_access = acc;
			cff_kind = kind
		};
	};

	public static function convert_ilevent(ctx, p, ev) return {
		var p = { (p) with pfile = ^ (p.pfile, ^ (", [", ^ (ev.ename, "]"))) };
		var name = ev.ename;
		var kind = FVar(Some(convert_signature(ctx, p, ev.esig.snorm)), None);
		var meta = ::((new Tuple(Meta.Event, [], p)), ::((new Tuple(Meta.Keep, [], p)), ::((new Tuple(Meta.SkipReflection, [], p)), [])));
		var acc = ::(APrivate, []);
		function add_m(acc, m) return {
			switch (m) {
			case None: acc;
			case Some(name, flags): if (List.mem(CMStatic, flags.mf_contract)) {
					::(AStatic, acc);
				} else {
					acc;
				};
			};
		};
		if (PMap.mem("net_loader_debug", ctx.ncom.defines)) {
			Printf.printf("\tevent %s : %s\n", name, IlMetaDebug.ilsig_s(ev.esig.ssig));
		} else {
			[];
		};
		var acc = add_m(acc, ev.eadd);
		var acc = add_m(acc, ev.eremove);
		var acc = add_m(acc, ev.eraise);
		{
			() with cff_name = name;
			cff_doc = None;
			cff_pos = p;
			cff_meta = meta;
			cff_access = acc;
			cff_kind = kind
		};
	};

	public static function convert_ilmethod(ctx, p, m, is_explicit_impl) return {
		if ( && (!(Common.defined(ctx.ncom, Define.Unsafe)), has_unmanaged(m.msig.snorm))) {
			raise(Exit);
		} else {
			[];
		};
		var force_check = Common.defined(ctx.ncom, Define.ForceLibCheck);
		var p = { (p) with pfile = ^ (p.pfile, ^ (", [", ^ (m.mname, "]"))) };
		var cff_doc = None;
		var cff_pos = p;
		var cff_name = switch (m.mname) {
		case .ctor: "new";
		case .cctor: raise(Exit);
		case Equals | GetHashCode: raise(Exit);
		case name if (>(String.length(name), 5)):
			switch (String.sub(name, 0, 5)) {
			case __hx_: raise(Exit);
			case _: name;
			};
		case name: name;
		};
		var acc = switch (m.mflags.mf_access) {
		case FAFamily | FAFamOrAssem: APrivate;
		case FAPublic if (||(List.mem(SGetter, m.msemantics), List.mem(SSetter, m.msemantics))): APrivate;
		case FAPublic: APublic;
		case _: if (PMap.mem("net_loader_debug", ctx.ncom.defines)) {
				Printf.printf("\tmethod %s [skipped] : %s\n", cff_name, IlMetaDebug.ilsig_s(m.msig.ssig));
			} else {
				[];
			};
			raise(Exit);
		};
		var is_static = ref(False);
	var Tuple(acc, is_final) = List.fold_left(function (acc, is_final): function case CMStatic if (<>(cff_name, "new")): is_static.val = True;
									   (new Tuple(::(AStatic, acc), is_final));
								   case CMVirtual if (=(is_final, None)): (new Tuple(acc, Some(False)));
										   case CMFinal: (new Tuple(acc, Some(True)));
											   case _: (new Tuple(acc, is_final)), (new Tuple(::(acc, []), None)), m.mflags.mf_contract);
		if (PMap.mem("net_loader_debug", ctx.ncom.defines)) {
			Printf.printf("\t%smethod %s : %s\n", if (is_static.val) {
			"static ";
		} else {
			"";
		}, cff_name, IlMetaDebug.ilsig_s(m.msig.ssig));
		} else {
			[];
		};
		var meta = ::((new Tuple(Meta.Overload, [], p)), []);
		var meta = switch (is_final) {
		case None | Some(True) if (!(force_check)): ::((new Tuple(Meta.Final, [], p)), meta);
		case _: meta;
		};
		var meta = if (is_explicit_impl) {
			::((new Tuple(Meta.NoCompletion, [], p)), ::((new Tuple(Meta.SkipReflection, [], p)), meta));
		} else {
			meta;
		};
		function change_sig(match) return switch (match) {
		case LManagedPointer(s): LManagedPointer(change_sig(s));
		case LPointer(s): LPointer(change_sig(s));
		case LValueType(p, pl): LValueType(p, List.map(change_sig, pl));
		case LClass(p, pl): LClass(p, List.map(change_sig, pl));
		case LTypeParam(i): LObject;
		case LVector(s): LVector(change_sig(s));
		case LArray(s, a): LArray(change_sig(s), a);
		case LMethod(c, r, args): LMethod(c, change_sig(r), List.map(change_sig, args));
		case p: p;
		};
		var change_sig = if (is_static.val) {
			change_sig;
		} else {
			function s: s;
		};
		var ret = if ( && ( > (String.length(cff_name), 4), = (String.sub(cff_name, 0, 4), "set_"))) {
			switch ((new Tuple(m.mret.snorm, m.margs))) {
			case (LVoid, ::((_, _, s), [])): s.snorm;
			case _: m.mret.snorm;
			};
		} else {
			m.mret.snorm;
		};
		var kind = var args = List.map(function (name, flag, s): var t = switch (s.snorm) {
	case LManagedPointer(s): var is_out = && (List.mem(POut, flag.pf_io), !(List.mem(PIn, flag.pf_io)));
			var name = if (is_out) {
				"Out";
			} else {
				"Ref";
			};
			mk_type_path(ctx, (new Tuple(::("cs", []), [], name)), ::(TPType(convert_signature(ctx, p, s)), []));
		case _: convert_signature(ctx, p, change_sig(s.snorm));
		};
		(new Tuple(name, False, Some(t), None)), m.margs);
		var ret = convert_signature(ctx, p, change_sig(ret));
		var types = List.map(function t: { () with tp_name = ^ ("M", string_of_int(t.tnumber));
										   tp_params = [];
										   tp_constraints = [];
										   tp_meta = []
										 }, m.mtypes);
		FFun({ () with f_params = types;
			   f_args = args;
			   f_type = Some(ret);
			   f_expr = None
			 });
		var Tuple(cff_name, cff_meta) = if ( = (String.get(cff_name, 0), '%')) {
			var name = String.sub(cff_name, 1, -(String.length(cff_name), 1));
			(new Tuple( ^ ("_", name), ::((new Tuple(Meta.Native, ::((new Tuple(EConst(String(name)), cff_pos)), []), cff_pos)),
										  meta)));
		} else {
			(new Tuple(cff_name, meta));
		};
		var acc = switch (m.moverride) {
		case None: acc;
		case _ if (=(cff_name, "new")): acc;
		case Some(path, s): switch (lookup_ilclass(ctx.nstd, ctx.ncom, path)) {
			case Some(ilcls) if (!(List.mem(SInterface, ilcls.cflags.tdf_semantics))): ::(AOverride, acc);
			case None if (ctx.ncom.verbose): prerr_endline( ^ ("[net-lib] A referenced assembly for path ", ^ (ilpath_s(path),
						" was not found")));
				acc;
			case _: acc;
			};
		};
		{
			() with cff_name = cff_name;
			cff_doc = cff_doc;
			cff_pos = cff_pos;
			cff_meta = cff_meta;
			cff_access = acc;
			cff_kind = kind
		};
	};

	public static function convert_ilprop(ctx, p, prop, is_explicit_impl) return {
		if ( && (!(Common.defined(ctx.ncom, Define.Unsafe)), has_unmanaged(prop.psig.snorm))) {
			raise(Exit);
		} else {
			[];
		};
		var p = { (p) with pfile = ^ (p.pfile, ^ (", [", ^ (prop.pname, "]"))) };
		var pmflags = switch ((new Tuple(prop.pget, prop.pset))) {
		case (Some(_, fl1), _): Some(fl1);
		case (_, Some(_, fl2)): Some(fl2);
		case _: None;
		};
		var cff_access = switch (pmflags) {
		case Some({ mf_access = FAFamily | FAFamOrAssem }): APrivate;
		case Some({ mf_access = FAPublic }): APublic;
		case _: raise(Exit);
		};
		function access(acc) return {
			acc.mf_access;
		};
		var cff_access = switch (pmflags) {
		case Some(m) if (List.mem(CMStatic, m.mf_contract)): ::(AStatic, ::(cff_access, []));
		case _: ::(cff_access, []);
		};
		var get = switch (prop.pget) {
		case None: "never";
		case Some(s, _) if (||(<=(String.length(s), 4), <>(String.sub(s, 0, 4), "get_"))): raise(Exit);
		case Some(_, m) if (<>(access(m), FAPublic)):
			switch (access(m)) {
			case FAFamily | FAFamOrAssem: "null";
			case _: "never";
			};
		case Some(_): "get";
		};
		var set = switch (prop.pset) {
		case None: "never";
		case Some(s, _) if (||(<=(String.length(s), 4), <>(String.sub(s, 0, 4), "set_"))): raise(Exit);
		case Some(_, m) if (<>(access(m), FAPublic)):
			switch (access(m)) {
			case FAFamily | FAFamOrAssem: "never";
			case _: "never";
			};
		case Some(_): "set";
		};
		if (PMap.mem("net_loader_debug", ctx.ncom.defines)) {
			Printf.printf("\tproperty %s [%s,%s] : %s\n", prop.pname, get, set, IlMetaDebug.ilsig_s(prop.psig.ssig));
		} else {
			[];
		};
		var ilsig = switch (prop.psig.snorm) {
		case LMethod(_, ret, []): ret;
		case s: raise(Exit);
		};
		var meta = if (is_explicit_impl) {
			::((new Tuple(Meta.NoCompletion, [], p)), ::((new Tuple(Meta.SkipReflection, [], p)), []));
		} else {
			[];
		};
		var kind = FProp(get, set, Some(convert_signature(ctx, p, ilsig)), None);
		{
			() with cff_name = prop.pname;
			cff_doc = None;
			cff_pos = p;
			cff_meta = meta;
			cff_access = cff_access;
			cff_kind = kind
		};
	};

	public static function get_type_path(ctx, ct) return {
		switch (ct) {
		case CTPath(p): p;
		case _: assert False;
		};
	};

	public static function is_explicit(ctx, ilcls, i) return {
		var s = switch (i) {
		case LClass(path, _) | LValueType(path, _): ilpath_s(path);
		case _: assert False;
		};
		var len = String.length(s);
		List.exists(function m: && ( > (String.length(m.mname), len), = (String.sub(m.mname, 0, len), s)), ilcls.cmethods);
	};

	public static function mke(e, p) return {
		(new Tuple(e, p));
	};

	public static function mk_special_call(name, p, args) return {
		mke(ECast(mke(EUntyped(mke(ECall(mke(EConst(Ident(name)), p), args), p)), p), None), p);
	};

	public static function mk_this_call(name, p, args) return {
		mke(ECall(mke(EField(mke(EConst(Ident("this")), p), name), p), args), p);
	};

	public static function mk_metas(metas, p) return {
		List.map(function m: (new Tuple(m, [], p)), metas);
	};

	public static function mk_abstract_fun(name, p, kind, metas, acc) return {
		var metas = mk_metas(metas, p);
		{
			() with cff_name = name;
			cff_doc = None;
			cff_pos = p;
			cff_meta = metas;
			cff_access = acc;
			cff_kind = kind
		};
	};

	public static function convert_fun_arg(ctx, p) return {
	case LManagedPointer(s): mk_type_path(ctx, (new Tuple(::("cs", []), [], "Ref")), ::(TPType(convert_signature(ctx, p, s)), []));
	case s: convert_signature(ctx, p, s);
	};

	public static function convert_fun(ctx, p, ret, args) return {
		var args = List.map(convert_fun_arg(ctx, p), args);
		CTFunction(args, convert_signature(ctx, p, ret));
	};

	public static function get_clsname(ctx, cpath) return {
		switch (netpath_to_hx(ctx.nstd, cpath)) {
			(_, n): n;
		};
	};

	public static function convert_delegate(ctx, p, ilcls) return {
		var p = { (p) with pfile = ^ (p.pfile, ", [abstract delegate]") };
		var abs_type = mk_type_path(ctx, ilcls.cpath, List.map(function t: TPType(mk_type_path(ctx, (new Tuple([], [], ^ ("T", string_of_int(t.tnumber)))), [])), ilcls.ctypes));
		var invoke = List.find(function m: = (m.mname, "Invoke"), ilcls.cmethods);
		var ret = invoke.mret.snorm;
		var args = List.map(function (_, _, s): s.snorm, invoke.margs);
		var haxe_type = convert_fun(ctx, p, ret, args);
		var types = List.map(function t: { () with tp_name = ^ ("T", string_of_int(t.tnumber));
										   tp_params = [];
										   tp_constraints = [];
										   tp_meta = []
										 }, ilcls.ctypes);
		function mk_op_fn(op, name, p) return {
			var fn_name = List.assoc(op, cs_binops);
			var clsname = switch (ilcls.cpath) {
				(ns, inner, n): get_clsname(ctx, (new Tuple(ns, inner, ^ ("Delegate_", n))));
			};
			var expr = (new Tuple(ECall((new Tuple(EField((new Tuple(EConst(Ident(clsname)), p)), fn_name), p)), ::((new Tuple(EConst(Ident("arg1")), p)), ::((new Tuple(EConst(Ident("arg2")), p)), []))), p));
			FFun({ () with f_params = types;
				   f_args = ::((new Tuple("arg1", False, Some(abs_type), None)), ::((new Tuple("arg2", False, Some(abs_type), None)), []));
				   f_type = Some(abs_type);
				   f_expr = Some(EReturn(Some(expr)), p)
				 });
		};
		function mk_op(op, name) return {
			var p = { (p) with pfile = ^ (p.pfile, ^ (", [op ", ^ (name, "]"))) };
			{
				() with cff_name = name;
				cff_doc = None;
				cff_pos = p;
				cff_meta = ::((new Tuple(Meta.Extern, [], p)), ::((new Tuple(Meta.Op, ::((new Tuple(EBinop(op, (new Tuple(EConst(Ident("A")), p)), (new Tuple(EConst(Ident("B")), p))), p)), []), p)), []));
				cff_access = ::(APublic, ::(AInline, ::(AStatic, [])));
				cff_kind = mk_op_fn(op, name, p)
			};
		};
		var params = List.map(function s: TPType(mk_type_path(ctx, (new Tuple([], [], s.tp_name)), [])), types);
		var underlying_type = switch (ilcls.cpath) {
			(ns, inner, name): mk_type_path(ctx, (new Tuple(ns, inner, ^ ("Delegate_", name))), params);
		};
		var fn_new = FFun({ () with f_params = [];
							f_args = ::((new Tuple("hxfunc", False, Some(haxe_type), None)), []);
							f_type = None;
							f_expr = Some(EBinop(Ast.OpAssign, (new Tuple(EConst(Ident("this")), p)), mk_special_call("__delegate__", p, ::((new Tuple(EConst(Ident("hxfunc")), p)), []))), p)
						  });
		var fn_from_hx = FFun({ () with f_params = types;
								f_args = ::((new Tuple("hxfunc", False, Some(haxe_type), None)), []);
								f_type = Some(mk_type_path(ctx, ilcls.cpath, params));
								f_expr = Some(EReturn(Some(mk_special_call("__delegate__", p, ::((new Tuple(EConst(Ident("hxfunc")), p)), [])))), p)
							  });
		var fn_asdel = FFun({ () with f_params = [];
							  f_args = [];
							  f_type = None;
							  f_expr = Some(EReturn(Some(EConst(Ident("this")), p)), p)
							});
		var fn_new = mk_abstract_fun("new", p, fn_new, ::(Meta.Extern, []), ::(APublic, ::(AInline, [])));
		var fn_from_hx = mk_abstract_fun("FromHaxeFunction", p, fn_from_hx, ::(Meta.Extern, ::(Meta.From, [])), ::(APublic, ::(AInline, ::(AStatic, []))));
		var fn_asdel = mk_abstract_fun("AsDelegate", p, fn_asdel, ::(Meta.Extern, []), ::(APublic, ::(AInline, [])));
		var Tuple(_, c) = netpath_to_hx(ctx.nstd, ilcls.cpath);
		EAbstract({ () with d_name = netname_to_hx(c);
					d_doc = None;
					d_params = types;
					d_meta = mk_metas(::(Meta.Delegate, ::(Meta.Forward, [])), p);
					d_flags = ::(AIsType(underlying_type), []);
					d_data = ::(fn_new, ::(fn_from_hx, ::(fn_asdel, ::(mk_op(Ast.OpAdd, "Add"), ::(mk_op(Ast.OpSub, "Remove"), [])))))
				  });
	};

	public static function convert_ilclass(ctx, p, ? : (delegate = False), ilcls) return {
		switch (ilcls.csuper) {
		case Some({ snorm = LClass((::(System, []), [], Enum), []) }): convert_ilenum(ctx, p, ilcls);
		case _: var flags = ref(::(HExtern, []));
			if (PMap.mem("net_loader_debug", ctx.ncom.defines)) {
				var sup = switch (ilcls.csuper) {
				case None: [];
				case Some(c): ::(IlMetaDebug.ilsig_s(c.ssig), []);
				};
				var sup = @(sup, List.map(function i: IlMetaDebug.ilsig_s(i.ssig), ilcls.cimplements));
				print_endline( ^ ("converting ", ^ (ilpath_s(ilcls.cpath), ^ (" : ", String.concat(", ", sup)))));
			} else {
				[];
			};
			var meta = ref(::((new Tuple(Meta.CsNative, [], p)), ::((new Tuple(Meta.Native,
							  ::((new Tuple(EConst(String(ilpath_s(ilcls.cpath))), p)), []), p)), [])));
			var force_check = Common.defined(ctx.ncom, Define.ForceLibCheck);
			if (!(force_check)) {
				meta.val = ::((new Tuple(Meta.LibType, [], p)), meta.val);
			} else {
				[];
			};
			var is_interface = ref(False);
			List.iter(function f:
			switch (f) {
		case SSealed: meta.val = ::((new Tuple(Meta.Final, [], p)), meta.val);
			case SInterface: is_interface.val = True;
				flags.val = ::(HInterface, flags.val);
			case SAbstract: meta.val = ::((new Tuple(Meta.Abstract, [], p)), meta.val);
			case _: [];
			}, ilcls.cflags.tdf_semantics);
			switch (ilcls.csuper) {
			case Some({ snorm = LClass((::(System, []), [], Object), []) }): [];
			case Some({ snorm = LClass((::(System, []), [], ValueType), []) } = s): flags.val = ::(HExtends(get_type_path(ctx,
						convert_signature(ctx, p, s.snorm))), flags.val);
				meta.val = ::((new Tuple(Meta.Struct, [], p)), meta.val);
			case Some({ snorm = LClass((::(haxe, ::(lang, [])), [], HxObject), []) }): meta.val = ::((new Tuple(Meta.HxGen, [], p)),
						meta.val);
			case Some(s): flags.val = ::(HExtends(get_type_path(ctx, convert_signature(ctx, p, s.snorm))), flags.val);
			case _: [];
			};
			var has_explicit_ifaces = ref(False);
			List.iter(function i:
			switch (i.snorm) {
		case LClass((::(haxe, ::(lang, [])), [], IHxObject), _): meta.val = ::((new Tuple(Meta.HxGen, [], p)), meta.val);
			case i: if (is_explicit(ctx, ilcls, i)) {
					has_explicit_ifaces.val = True;
				} else {
					[];
				};
				flags.val = if (is_interface.val) {
					::(HExtends(get_type_path(ctx, convert_signature(ctx, p, i))), flags.val);
				} else {
					::(HImplements(get_type_path(ctx, convert_signature(ctx, p, i))), flags.val);
				};
			}, ilcls.cimplements);
			if ( && (has_explicit_ifaces.val, force_check)) {
				meta.val = ::((new Tuple(Meta.LibType, [], p)), meta.val);
			} else {
				[];
			};
		ignore(List.exists(function case {
				psig = { snorm = LMethod(_, ret, ::(v, [])) }
			}: flags.val = if (is_interface.val) {
			::(HExtends(raw_type_path(ctx, (new Tuple([], "ArrayAccess")), ::(TPType(convert_signature(ctx, p, ret)), []))),
			   flags.val);
			} else {
				::(HImplements(raw_type_path(ctx, (new Tuple([], "ArrayAccess")), ::(TPType(convert_signature(ctx, p, ret)), []))),
				   flags.val);
			};
			True;
		case _: False, ilcls.cprops));
			var fields = ref([]);
			function run_fields(fn, f) return {
				List.iter(function f:
				try {
					fields.val = ::(fn(f), fields.val);
				} catch (e: Exit) {
					[];
				}, f);
			};
			var meths = if (is_interface.val) {
				List.filter(function m: = (m.moverride, None), ilcls.cmethods);
			} else {
				ilcls.cmethods;
			};
			run_fields(function m: convert_ilmethod(ctx, p, m, List.exists(function m2: && ( != (m, m2),
													&& (<>(String.get(m2.mname, 0), '.'), String.ends_with(m2.mname, ^ (".", m.mname)))), meths)), meths);
			run_fields(convert_ilfield(ctx, p), ilcls.cfields);
			run_fields(function prop: convert_ilprop(ctx, p, prop, List.exists(function p2: && ( != (prop, p2),
					   && (<>(String.get(p2.pname, 0), '.'), String.ends_with(p2.pname, ^ (".", prop.pname)))), ilcls.cprops)), ilcls.cprops);
			run_fields(convert_ilevent(ctx, p), ilcls.cevents);
			var params = List.map(function p: {
									  () with tp_name = ^ ("T", string_of_int(p.tnumber));
									  tp_params = [];
									  tp_constraints = [];
									  tp_meta = []
								  }, ilcls.ctypes);
			if (delegate) {
				var path = ilcls.cpath;
				var thist = mk_type_path(ctx, path, List.map(function t: TPType(mk_type_path(ctx, (new Tuple([], [], ^ ("T",
										 string_of_int(t.tnumber)))), [])), ilcls.ctypes));
				function op(name) return {
					{
						() with cff_name = name;
						cff_doc = None;
						cff_pos = p;
						cff_meta = [];
						cff_access = ::(APublic, ::(AStatic, []));
						cff_kind = FFun({
							() with f_params = params;
							f_args = ::((new Tuple("arg1", False, Some(thist), None)), ::((new Tuple("arg2", False, Some(thist), None)), []));
							f_type = Some(thist);
							f_expr = None
						})
					};
				};
				fields.val = ::(op("op_Addition"), ::(op("op_Subtraction"), fields.val));
			} else {
				[];
			};
			var path = switch (ilcls.cpath) {
			case (ns, inner, name) if (delegate): (new Tuple(ns, inner, ^ ("Delegate_", name)));
			case _: ilcls.cpath;
			};
			var Tuple(_, c) = netpath_to_hx(ctx.nstd, path);
			EClass({ () with d_name = netname_to_hx(c);
					 d_doc = None;
					 d_params = params;
					 d_meta = meta.val;
					 d_flags = flags.val;
					 d_data = fields.val
				   });
		};
	};

	public static function get_fname(match) return switch (match) {
	case IlField(f): f.fname;
	case IlMethod(m): m.mname;
	case IlProp(p): p.pname;
	};

	public static function is_static(match) return switch (match) {
	case IlField(f): List.mem(CStatic, f.fflags.ff_contract);
	case IlMethod(m): List.mem(CMStatic, m.mflags.mf_contract);
	case IlProp(p): List.exists(function case None: False;
									case Some(_, m): List.mem(CMStatic, m.mf_contract), ::(p.pget, ::(p.pset, [])));
	};

	public static function change_name(name) return {
	case IlField(f): IlField({ (f) with fname = name });
	case IlMethod(m): IlMethod({ (m) with mname = name });
	case IlProp(p): IlProp({ (p) with pname = name });
	};

	public static function compatible_methods(m1, m2) return {
		switch ((new Tuple(m1, m2))) {
		case (IlMethod({ msig = { snorm = LMethod(_, ret1, args1) } }), IlMethod({ msig = { snorm = LMethod(_, ret2, args2) } })):
			&& ( = (ret1, ret2), = (args1, args2));
		case _: False;
		};
	};

	public static function ilcls_from_ilsig(ctx, ilsig) return {
		var Tuple(path, params) = switch (ilsig) {
		case LClass(path, params) | LValueType(path, params): (new Tuple(path, params));
		case LObject: (new Tuple((new Tuple(::("System", []), [], "Object")), []));
		case _: raise(Not_found);
		};
		switch (lookup_ilclass(ctx.nstd, ctx.ncom, path)) {
		case None: raise(Not_found);
		case Some(c): (new Tuple(c, params));
		};
	};

	public static function ilapply_params(params) return {
	case LManagedPointer(s): LManagedPointer(ilapply_params(params, s));
	case LPointer(s): LPointer(ilapply_params(params, s));
	case LValueType(p, pl): LValueType(p, List.map(ilapply_params(params), pl));
	case LClass(p, pl): LClass(p, List.map(ilapply_params(params), pl));
	case LTypeParam(i): List.nth(params, i);
	case LVector(s): LVector(ilapply_params(params, s));
	case LArray(s, a): LArray(ilapply_params(params, s), a);
	case LMethod(c, r, args): LMethod(c, ilapply_params(params, r), List.map(ilapply_params(params), args));
	case p: p;
	};

	public static function ilcls_with_params(ctx, cls, params) return {
		switch (cls.ctypes) {
		case []: cls;
		case _: {
			(cls) with cfields = List.map(function f: {
				(f) with fsig = { (f.fsig) with snorm = ilapply_params(params, f.fsig.snorm) }
			}, cls.cfields);
			cmethods = List.map(function m: {
									(m) with msig = { (m.msig) with snorm = ilapply_params(params, m.msig.snorm) };
									margs = List.map(function (n, f, s): (new Tuple(n, f, { (s) with snorm = ilapply_params(params, s.snorm) })), m.margs);
									mret = { (m.mret) with snorm = ilapply_params(params, m.mret.snorm) }
								}, cls.cmethods);
			cprops = List.map(function p: {
								  (p) with psig = { (p.psig) with snorm = ilapply_params(params, p.psig.snorm) }
							  }, cls.cprops);
			csuper = Option.map(function s: {
									(s) with snorm = ilapply_params(params, s.snorm)
								}, cls.csuper);
			cimplements = List.map(function s: {
									   (s) with snorm = ilapply_params(params, s.snorm)
								   }, cls.cimplements)
		};
		};
	};

	public static function compatible_params(t1, t2) return {
		switch ((new Tuple(t1, t2))) {
		case (LManagedPointer(s1), LManagedPointer(s2)): compatible_params(s1, s2);
		case (LManagedPointer(s1), s2) | (s1, LManagedPointer(s2)): compatible_params(s1, s2);
		case _: = (t1, t2);
		};
	};

	public static function compatible_methods(m1, m2) return {
		switch ((new Tuple(m1, m2))) {
		case (LMethod(_, r1, a1), LMethod(_, r2, a2)): try {
				List.for_all2(function a1: function a2: compatible_params(a1, a2), a1, a2);
			} catch (e: Invalid_argument(_)) {
				False;
			};
		case _: False;
		};
	};

	public static function compatible_field(f1, f2) return {
		switch ((new Tuple(f1, f2))) {
		case (IlMethod({ msig = { snorm = LMethod(_, _, a1) } }), IlMethod({ msig = { snorm = LMethod(_, _, a2) } })): = (a1, a2);
		case (IlProp(p1), IlProp(p2)): True;
		case (IlField(f1), IlField(f2)): True;
		case _: False;
		};
	};

	public static function get_all_fields(cls) return {
		var all_fields = List.map(function f: (new Tuple(IlField(f), cls.cpath, f.fname, List.mem(CStatic, f.fflags.ff_contract))), cls.cfields);
		var all_fields = @(all_fields, List.map(function m: (new Tuple(IlMethod(m), cls.cpath, m.mname, List.mem(CMStatic, m.mflags.mf_contract))), cls.cmethods));
		var all_fields = @(all_fields, List.map(function p: (new Tuple(IlProp(p), cls.cpath, p.pname, is_static(IlProp(p)))), cls.cprops));
		all_fields;
	};

	public static function normalize_ilcls(ctx, cls) return {
		var force_check = Common.defined(ctx.ncom, Define.ForceLibCheck);
		function loop(acc) return {
		case []: acc;
		case ::(m, cmeths): var static = List.mem(CMStatic, m.mflags.mf_contract);
			if (List.exists(function m2: && ( = (m.mname, m2.mname), && ( = (List.mem(CMStatic, m2.mflags.mf_contract), static), compatible_methods(m.msig.snorm, m2.msig.snorm))), cmeths)) {
				loop(acc, cmeths);
			} else {
				loop(::(m, acc), cmeths);
			};
		};
		var meths = loop([], cls.cmethods);
		var meths = List.map(function v: ref(v), meths);
		var no_overrides = List.filter(function m: var m = m.val;
									   !(List.mem(CMStatic, m.mflags.mf_contract)), meths);
		var no_overrides = ref(no_overrides);
		var all_fields = ref([]);
		var all_events_name = Hashtbl.create(0);
		function add_cls_events_collision(cls) return {
			List.iter(function m:
			if (!(List.mem(CMStatic, m.mflags.mf_contract))) {
			Hashtbl.replace(all_events_name, m.mname, True);
			} else {
				[];
			}, cls.cmethods);
			List.iter(function p:
			if (!(is_static(IlProp(p)))) {
			Hashtbl.replace(all_events_name, p.pname, True);
			} else {
				[];
			}, cls.cprops);
		};
		function loop(cls) return {
			try {
				switch (cls.csuper) {
				case Some({ snorm = LClass((::(System, []), [], Object), _) }) | Some({ snorm = LObject }) | None: [];
				case Some(s): var Tuple(cls, params) = ilcls_from_ilsig(ctx, s.snorm);
					var cls = ilcls_with_params(ctx, cls, params);
					if (force_check) {
						no_overrides.val = List.filter(function v: var m = v.val;
													   var is_override_here = List.exists(function m2: && ( = (m2.mname, m.mname),
															   && (!(List.mem(CMStatic, m2.mflags.mf_contract)), compatible_methods(m.msig.snorm, m2.msig.snorm))), cls.cmethods);
						if (is_override_here) {
						v.val = { (m) with moverride = Some(cls.cpath, m.mname) };
					} else {
						[];
						};
						!(is_override_here), no_overrides.val);
					} else {
						[];
					};
					all_fields.val = @(get_all_fields(cls), all_fields.val);
					add_cls_events_collision(cls);
					List.iter(function ev: Hashtbl.replace(all_events_name, ev.ename, True), cls.cevents);
					loop(cls);
				};
			} catch (e: Not_found) {
				[];
			};
		};
		loop(cls);
		add_cls_events_collision(cls);
		if (force_check) {
			List.iter(function v: v.val = { (v.val) with moverride = None }, no_overrides.val);
		} else {
			[];
		};
		var added = ref([]);
		var current_all = ref(@(get_all_fields(cls), all_fields.val));
		function loop_interface(cls, iface) return {
			try {
				switch (iface.snorm) {
				case LClass((::(System, []), [], Object), _) | LObject: [];
				case LClass(path, _) if (=(path, cls.cpath)): [];
				case s: var Tuple(cif, params) = ilcls_from_ilsig(ctx, s);
					var cif = ilcls_with_params(ctx, cif, params);
				List.iter(function case (f, _, name, False) = ff: if (!(List.exists(function
					case (f2, _, name2, False) if (||(=(name, name2), String.ends_with(name2, ^(".", name)))): compatible_field(f, f2);
							case _: False, current_all.val))) {
									current_all.val = ::(ff, current_all.val);
										added.val = ::(ff, added.val);
									} else {
									List.iter(function mref:
									switch (mref.val) {
								case m if (&&(=(m.mname, name), compatible_field(f, IlMethod(m)))): mref.val = { (m) with mflags = { (m.mflags) with mf_access = FAPublic } };
									case _: [];
									}, meths);
								};
				case _: [], get_all_fields(cif));
					List.iter(loop_interface(cif), cif.cimplements);
				};
			} catch (e: Not_found) {
				[];
			};
		};
		List.iter(loop_interface(cls), cls.cimplements);
	var added = List.map(function case (IlMethod(m), a, name, b) if (<>(m.mflags.mf_access, FAPublic)): (new Tuple(IlMethod({ (m) with mflags = { (m.mflags) with mf_access = FAPublic } }), a, name, b));
							 case (IlField(f), a, name, b) if (<>(f.fflags.ff_access, FAPublic)): (new Tuple(IlField({ (f) with fflags = { (f.fflags) with ff_access = FAPublic } }), a, name, b));
									 case s: s, added.val);
		var props = if (force_check) {
			List.filter(function p: var static = is_static(IlProp(p));
						var name = p.pname;
					!(List.exists(function case (IlProp(_), _, n, s): && ( = (s, static), = (name, n));
									  case _: False, all_fields.val)), cls.cprops);
		} else {
			cls.cprops;
		};
		var cls = { (cls) with cmethods = List.map(function v: v.val, meths);
					cprops = props
				  };
		var clsfields = @(get_all_fields(cls), added);
		var super_fields = all_fields.val;
		all_fields.val = @(clsfields, all_fields.val);
		var refclsfields = List.map(function v: ref(v), clsfields);
		function fold_field(acc, v) return {
			var Tuple(f, p, name, is_static) = v.val;
			var Tuple(change, copy) = switch (name) {
			case _ if (is_haxe_keyword(name)): (new Tuple(True, False));
			case _: (new Tuple( || ( && (is_static, List.exists(function case (f, _, n, False): = (name, n);
				case _: False, all_fields.val)), && (!(is_static), switch (f) {
					case IlMethod(_): || (List.exists(function case (IlProp(_) | IlField(_), _, n, False): = (name, n);
							case _: False, super_fields), List.exists(function case (IlProp(_) | IlField(_), _, n, s): = (name, n);
									case _: False, clsfields));
						case _: False;
						})), True));
			};
			if (change) {
				var name = ^ ("%", name);
				var changed = (new Tuple(change_name(name, f), p, name, is_static));
				if (!(copy)) {
					v.val = changed;
				} else {
					[];
				};
				if (copy) {
					::(v, ::(ref(changed), acc));
				} else {
					::(v, acc);
				};
			} else {
				::(v, acc);
			};
		};
		var refclsfields = List.fold_left(fold_field, [], refclsfields);
		function fold(Tuple(fields, methods, props), f) return {
			switch (f.val) {
			case (IlField(f), _, _, _): (new Tuple(::(f, fields), methods, props));
			case (IlMethod(m), _, _, _): (new Tuple(fields, ::(m, methods), props));
			case (IlProp(p), _, _, _): (new Tuple(fields, methods, ::(p, props)));
			};
		};
		var Tuple(fields, methods, props) = List.fold_left(fold, (new Tuple([], [], [])), refclsfields);
		{
			(cls) with cfields = fields;
			cprops = props;
			cmethods = methods;
			cevents = List.filter(function ev: !(Hashtbl.mem(all_events_name, ev.ename)), cls.cevents)
		};
	};

	public static function add_net_std(com, file) return {
		com.net_std = ::(file, com.net_std);
	};

	public static function add_net_lib(com, file, std) return {
		var ilctx = ref(None);
		var netpath_to_hx = netpath_to_hx(std);
		var real_file = ref(file);
		function get_ctx([]) return {
			switch (ilctx.val) {
			case Some(c): c;
			case None: var file = if (Sys.file_exists(file)) {
					file;
				} else {
					try {
						Common.find_file(com, file);
					} catch (e: Not_found) {
						try {
							Common.find_file(com, ^ (file, ".dll"));
						} catch (e: Not_found) {
							failwith( ^ (".NET lib ", ^ (file, " not found")));
						};
					};
				};
				real_file.val = file;
				var r = PeReader.create_r(open_in_bin(file), com.defines);
				var ctx = PeReader.read(r);
				var clr_header = PeReader.read_clr_header(ctx);
				var cache = IlMetaReader.create_cache([]);
				var meta = IlMetaReader.read_meta_tables(ctx, clr_header, cache);
				close_in(r.PeReader.ch);
				if (PMap.mem("net_loader_debug", com.defines)) {
					print_endline( ^ ("for lib ", file));
				} else {
					[];
				};
				var il_typedefs = Hashtbl.copy(meta.il_typedefs);
				Hashtbl.clear(meta.il_typedefs);
				Hashtbl.iter(function _: function td: var path = IlMetaTools.get_path(TypeDef(td));
				if (PMap.mem("net_loader_debug", com.defines)) {
				Printf.printf("found %s\n", path_s(netpath_to_hx(path)));
				} else {
					[];
				};
				Hashtbl.replace(com.net_path_map, netpath_to_hx(path), path);
				Hashtbl.replace(meta.il_typedefs, path, td), il_typedefs);
				var meta = { () with nstd = std;
							 ncom = com;
							 nil = meta
						   };
				ilctx.val = Some(meta);
				meta;
			};
		};
		var cache = Hashtbl.create(0);
		function lookup(path) return {
			try {
				Hashtbl.find(cache, path);
			} catch (e: Not_found) {
				try {
					var ctx = get_ctx([]);
					var Tuple(ns, n, cl) = hxpath_to_net(ctx, path);
					var cls = IlMetaTools.convert_class(ctx.nil, (new Tuple(ns, n, cl)));
					var cls = normalize_ilcls(ctx, cls);
					Hashtbl.add(cache, path, Some(cls));
					Some(cls);
				} catch (e: Not_found) {
					Hashtbl.add(cache, path, None);
					None;
				};
			};
		};
		function all_files([]) return {
			Hashtbl.fold(function path: function _: function acc:
			switch (path) {
		case (_, ::(_, _), _): acc;
			case _: ::(netpath_to_hx(path), acc);
			}, get_ctx([]).nil.il_typedefs, []);
		};
		function build(path) return {
			var p = {
				() with pfile = ^ (real_file.val, ^ (" @ ", path_s(path)));
				pmin = 0;
				pmax = 0
			};
			var pack = switch (fst(path)) {
			case ::(haxe, ::(root, [])): [];
			case p: p;
			};
			var cp = ref([]);
			function build(path) return {
				try {
					if (PMap.mem("net_loader_debug", com.defines)) {
						Printf.printf("looking up %s\n", path_s(path));
					} else {
						[];
					};
					switch (lookup(path)) {
					case Some({ csuper = Some({ snorm = LClass((::(System, []), [], Delegate | MulticastDelegate), _) }) } = cls) if (List.mem(SSealed, cls.cflags.tdf_semantics))
								: var ctx = get_ctx([]);
						var hxcls = convert_ilclass(ctx, p, delegate = True, cls);
						var delegate = convert_delegate(ctx, p, cls);
						cp.val = ::((new Tuple(hxcls, p)), ::((new Tuple(delegate, p)), cp.val));
						List.iter(function ilpath: var path = netpath_to_hx(ilpath);
								  build(path), cls.cnested);
					case Some(cls): var ctx = get_ctx([]);
						var hxcls = convert_ilclass(ctx, p, cls);
						cp.val = ::((new Tuple(hxcls, p)), cp.val);
						List.iter(function ilpath: var path = netpath_to_hx(ilpath);
								  build(path), cls.cnested);
					case _: [];
					};
				} catch (e: Not_found | Exit) {
					[];
				};
			};
			build(path);
			switch (cp.val) {
			case []: None;
			case cp: Some(real_file.val, (new Tuple(pack, cp)));
			};
		};
		function build(path, p) return {
			build(path);
		};
		com.load_extern_type = @(com.load_extern_type, ::(build, []));
		com.net_libs = ::((new Tuple(file, std, all_files, lookup)), com.net_libs);
	};

	public static function before_generate(com) return {
		var net_ver = try {
			int_of_string(PMap.find("net_ver", com.defines));
		} catch (e: Not_found) {
			Common.define_value(com, Define.NetVer, "20");
			20;
		};
		if ( < (net_ver, 20)) {
			failwith( ^ (".NET version is defined to target .NET ", ^ (string_of_int(net_ver),
						 ", but the compiler can only output code to versions equal or superior to .NET 2.0 [defined as 20]")));
		} else {
			[];
		};
		function loop(match) return switch (match) {
		case ::(v, acc) if (<=(v, net_ver)): Common.raw_define(com, ^ ("NET_", string_of_int(v)));
			loop(acc);
		case _: [];
		};
		loop(::(20, ::(21, ::(30, ::(35, ::(40, ::(45, [])))))));
		var net_target = try {
			String.lowercase(PMap.find("net_target", com.defines));
		} catch (e: Not_found) {
			"net";
		};
		Common.define_value(com, Define.NetTarget, net_target);
		Common.raw_define(com, net_target);
		var stds = switch (com.net_std) {
		case []: ::("netlib", []);
		case s: s;
		};
		var digraph = ^ (net_target, ^ ("-", string_of_int(net_ver)));
		var matched = ref([]);
		List.iter(function f:
		try {
			var f = Common.find_file(com, ^ (f, ^ ("/", digraph)));
			matched.val = ::((new Tuple(f, Unix.opendir(f))), matched.val);
		} catch (e: _) {
			[];
		}, stds);
		if ( = (matched.val, [])) {
			failwith( ^ ("No .NET std lib directory with the pattern '", ^ (digraph, ^ ("' was found in the -net-std search path. ",
			"Try updating the hxcs lib to the latest version, or specifying another -net-std path."))));
		} else {
			[];
		};
		List.iter(function (path, f): 	function loop([]) return {
			try {
				var f = Unix.readdir(f);
				var finsens = String.lowercase(f);
				if (String.ends_with(finsens, ".dll")) {
					add_net_lib(com, ^ (path, ^ ("/", f)), True);
				} else {
					[];
				};
				loop([]);
			} catch (e: End_of_file) {
				Unix.closedir(f);
			};
		};
		loop([]), matched.val);
		List.iter(function (_, _, _, lookup): ignore(lookup((new Tuple([], "")))), com.net_libs);
	}
}
;
