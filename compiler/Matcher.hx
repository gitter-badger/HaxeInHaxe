import Ast;
import Common;
import Type;
import Typecore;

typedef Pvar = Tuple2<Tvar, Pos>;

enum Con_def {
	CEnum(value: TenumTenum_field);
	CConst(value: Tconstant);
	CAny;
	CType(value: Module_type);
	CArray(value: Int);
	CFields(value: IntList<Tuple<String, Tclass_field>>);
	CExpr(value: Texpr);
};

typedef Con = {
	c_def : Con_def,
	c_type : T,
	c_pos : Pos
};

enum St_def {
	SVar(value: Tvar);
	SField(value: StTclass_field);
	SEnum(value: StTenum_fieldInt);
	SArray(value: StInt);
	STuple(value: StIntInt);
};

typedef St = {
	st_def : St_def,
	st_type : T,
	st_pos : Pos
};

enum Dt {
	Switch(value: StList<Tuple<Con, Dt>>);
	Bind(value: List<Tuple<Tuple<Tvar, Pos>, St>>Dt);
	Goto(value: Int);
	Expr(value: Int);
	Guard(value: IntDtOption<Dt>);
};

enum Pat_def {
	PAny;
	PVar(value: Pvar);
	PCon(value: ConList<Pat>);
	POr(value: PatPat);
	PBind(value: PvarPat);
	PTuple(value: Array<Pat>);
};

typedef Pat = {
	p_def : Pat_def,
	p_type : T,
	p_pos : Pos
};

typedef Out = {
	o_pos : Pos,
	o_id : Int,
	o_catch_all : Bool,
	o_num_paths : Int
};

typedef Pat_vec = Tuple<Array<Pat>, Out>;

typedef Pat_matrix = List<Pat_vec>;

typedef Pattern_ctx = {
	pc_locals : PMap<String, Pvar>,
	pc_sub_vars : Option<Pvar>,
	pc_reify : Bool,
	pc_is_complex : Bool
};

typedef Matcher = {
	ctx : Typer,
	need_val : Bool,
	dt_lut : DynArray<Dt>,
	dt_cache : Hashtbl<Dt, Int>,
	dt_count : Int,
	outcomes : List<Out>,
	toplevel_or : Bool,
	has_extractor : Bool,
	expr_map : PMap<Int, Tuple<Texpr, Option<Texpr>>>,
	is_exhaustive : Bool
};

enum Type_finiteness {
	Infinite;
	CompileTimeFinite;
	RunTimeFinite;
};

class /*exception*/ Not_exhaustive {

};

class /*exception*/ Unrecognized_pattern {

};

class Matcher {
	public static function arity(con) return {
		switch (con.c_def) {
		case CEnum(_, { ef_type = TFun(args, _) }): List.length(args);
		case CEnum(_): 0;
		case CConst(_): 0;
		case CType(mt): 0;
		case CArray(i): i;
		case CFields(i, _): i;
		case CExpr(_): 0;
		case CAny: 0;
		};
	};

	public static function mk_st(def, t, p) return {
		{
			() with st_def = def;
			st_type = t;
			st_pos = p
		};
	};

	public static function mk_out(mctx, id, e, eg, is_catch_all, p) return {
		var out = {
			() with o_pos = p;
			o_id = id;
			o_catch_all = is_catch_all;
			o_num_paths = 0
		};
		mctx.outcomes = ::(out, mctx.outcomes);
		mctx.expr_map = PMap.add(id, (new Tuple(e, eg)), mctx.expr_map);
		out;
	};

	public static function clone_out(mctx, out, p) return {
		var out = { (out) with o_pos = p };
		mctx.outcomes = ::(out, mctx.outcomes);
		out;
	};

	public static function get_guard(mctx, id) return {
		snd(PMap.find(id, mctx.expr_map));
	};

	public static function get_expr(mctx, id) return {
		fst(PMap.find(id, mctx.expr_map));
	};

	public static function mk_pat(pdef, t, p) return {
		{
			() with p_def = pdef;
			p_type = t;
			p_pos = p
		};
	};

	public static function mk_con(cdef, t, p) return {
		{
			() with c_def = cdef;
			c_type = t;
			c_pos = p
		};
	};

	public static function mk_con_pat(cdef, pl, t, p) return {
		mk_pat(PCon(mk_con(cdef, t, p), pl), t, p);
	};

	public static function mk_any(t, p) return {
		mk_pat(PAny, t, p);
	};

	public static var any = mk_any(t_dynamic, Ast.null_pos);

	public static var fake_tuple_type = TInst(mk_class(null_module, (new Tuple([], "-Tuple")), null_pos), []);

	public static function mk_type_pat(ctx, mt, t, p) return {
		function loop(match) return switch (match) {
		case TClassDecl(_): "Class";
		case TEnumDecl(_): "Enum";
		case TAbstractDecl(a) if (Meta.has(Meta.RuntimeValue, a.a_meta)): "Class";
		case TTypeDecl(t): switch (follow(monomorphs(t.t_params, t.t_type))) {
			case TInst(c, _): loop(TClassDecl(c));
			case TEnum(en, _): loop(TEnumDecl(en));
			case TAbstract(a, _): loop(TAbstractDecl(a));
			case _: error("Cannot use this type as a value", p);
			};
		case _: error("Cannot use this type as a value", p);
		};
		var tcl = Typeload.load_instance(ctx, { () with tname = loop(mt);
												tpackage = [];
												tsub = None;
												tparams = []
											  }, p, True);
		var t2 = switch (tcl) {
		case TAbstract(a, _): TAbstract(a, ::(mk_mono([]), []));
		case _: assert False;
		};
		unify(ctx, t, t2, p);
		mk_con_pat(CType(mt), [], t2, p);
	};

	public static function mk_subs(st, con) return {
		var map = switch (follow(st.st_type)) {
		case TInst(c, pl): apply_params(c.cl_params, pl);
		case TEnum(en, pl): apply_params(en.e_params, pl);
		case TAbstract(a, pl): apply_params(a.a_params, pl);
		case _: function t: t;
		};
		switch (con.c_def) {
		case CFields(_, fl): List.map(function (s, cf): mk_st(SField(st, cf), map(cf.cf_type), st.st_pos), fl);
		case CEnum(en, { ef_type = TFun(_) } = ef): 	function loop(t) return {
				switch (follow(t)) {
				case TEnum(_, pl): pl;
				case TAbstract({ a_path = ([], EnumValue) }, []): [];
				case TAbstract(a, pl): loop(Abstract.get_underlying_type(a, pl));
				case _: [];
				};
			};
			var pl = loop(con.c_type);
			switch (apply_params(en.e_params, pl, monomorphs(ef.ef_params, ef.ef_type))) {
			case TFun(args, r): ExtList.List.mapi(function i: function (_, _, t): mk_st(SEnum(st, ef, i), t, st.st_pos), args);
			case _: assert False;
			};
		case CArray(0): [];
		case CArray(i): var t = switch (follow(con.c_type)) {
			case TInst({ cl_path = ([], Array) }, ::(t, [])): t;
			case TDynamic(_) = t: t;
			case _: assert False;
			};
			ExtList.List.init(i, function i: mk_st(SArray(st, i), t, st.st_pos));
		case CEnum(_) | CConst(_) | CType(_) | CExpr(_) | CAny: [];
		};
	};

	public static function get_tuple_params(t) return {
		switch (t) {
		case TFun(tl, tr) if (==(tr, fake_tuple_type)): Some(tl);
		case _: None;
		};
	};

	public static var s_type = s_type(print_context([]));

	public static function s_con(con) return {
		switch (con.c_def) {
		case CEnum(_, ef): ef.ef_name;
		case CAny: "_";
		case CConst(c): s_const(c);
		case CType(mt): s_type_path(t_path(mt));
		case CArray(i): ^ ("[", ^ (string_of_int(i), "]"));
		case CFields(_, fl): String.concat(",", List.map(function (s, _): s, fl));
		case CExpr(e): s_expr(s_type, e);
		};
	};

	public static function s_pat(pat) return {
		switch (pat.p_def) {
		case PVar(v, _): v.v_name;
		case PCon(c, []): s_con(c);
		case PCon(c, pl): ^ (s_con(c), ^ ("[", ^ (String.concat(",", List.map(s_pat, pl)), "]")));
		case POr(pat1, pat2): ^ (s_pat(pat1), ^ (" | ", s_pat(pat2)));
		case PAny: "_";
		case PBind((v, _), pat): ^ (v.v_name, ^ ("=", s_pat(pat)));
		case PTuple(pl): ^ ("[", ^ (String.concat(" ", Array.to_list(Array.map(s_pat, pl))), "]"));
		};
	};

	public static function s_pat_vec(pl) return {
		String.concat(" ", Array.to_list(Array.map(s_pat, pl)));
	};

	public static function s_pat_matrix(pmat) return {
		String.concat("\n", List.map(function (pl, out): ^ (s_pat_vec(pl), ^ ("->", "")), pmat));
	};

	public static function st_args(l, r, v) return {
		^ (if ( > (l, 0)) {
		^ (String.concat(",", ExtList.List.make(l, "_")), ",");
		} else {
			"";
		}, ^ (v, if ( > (r, 0)) {
		^ (",", String.concat(",", ExtList.List.make(r, "_")));
		} else {
			"";
		}));
	};

	public static function s_st(st) return {
		switch (st.st_def) {
		case SVar(v): v.v_name;
		case SEnum(st, ef, i): ^ (s_st(st), ^ (".", ^ (ef.ef_name, ^ (".", string_of_int(i)))));
		case SArray(st, i): ^ (s_st(st), ^ ("[", ^ (string_of_int(i), "]")));
		case STuple(st, i, a): ^ ("[", ^ (st_args(i, -(-(a, i), 1), s_st(st)), "]"));
		case SField(st, cf): ^ (s_st(st), ^ (".", cf.cf_name));
		};
	};

	public static function unify_enum_field(en, pl, ef, t) return {
		var t2 = switch (follow(ef.ef_type)) {
		case TFun(_, r): r;
		case t2: t2;
		};
		var t2 = apply_params(en.e_params, pl, monomorphs(ef.ef_params, t2));
		Type.unify(t2, t);
	};

	public static function unify(ctx, a, b, p) return {
		try {
			unify_raise(ctx, a, b, p);
		} catch (e: Error(Unify(l))(p)) {
			error(error_msg(Unify(l)), p);
		};
	};

	public static function is_value_type(match) return switch (match) {
	case TMono(r): switch (r.val) {
		case None: False;
		case Some(t): is_value_type(t);
		};
	case TType(t, tl): is_value_type(apply_params(t.t_params, tl, t.t_type));
	case TInst({ cl_path = ([], String) }, []): True;
	case TAbstract(_): True;
	case _: False;
	};

	public static function matches_null(ctx, t) return {
		switch (t) {
		case TMono(r): switch (r.val) {
			case None: r.val = Some(ctx.t.tnull(mk_mono([])));
				True;
			case Some(t): matches_null(ctx, t);
			};
		case TType({ t_path = ([], Null) }, ::(_, [])): True;
		case TLazy(f): matches_null(ctx, f.val([]));
		case TType(t, tl): matches_null(ctx, apply_params(t.t_params, tl, t.t_type));
		case TFun(_): False;
		case TAbstract(a, _): !(Meta.has(Meta.NotNull, a.a_meta));
		case _: True;
		};
	};

	public static function to_pattern(ctx, e, t) return {
		function perror(p) return {
			error("Unrecognized pattern", p);
		};
		function verror(n, p) return {
			error( ^ ("Variable ", ^ (n, " must appear exactly once in each sub-pattern")), p);
		};
		function mk_var(tctx, s, t, p) return {
			var v = switch (tctx.pc_sub_vars) {
			case Some(vmap): fst(try {
					PMap.find(s, vmap);
				} catch (e: Not_found) {
					verror(s, p);
				});
			case None: alloc_var(s, t);
			};
			unify(ctx, t, v.v_type, p);
			if (PMap.mem(s, tctx.pc_locals)) {
				verror(s, p);
			} else {
				[];
			};
			tctx.pc_locals = PMap.add(s, (new Tuple(v, p)), tctx.pc_locals);
			v;
		};
		function check_texpr_pattern(e, t, p) return {
			var ec = switch (Optimizer.make_constant_expression(ctx, concat_strings = True, e)) {
			case Some(e): e;
			case None: e;
			};
			switch (ec.eexpr) {
			case TField(_, FEnum(en, ef)): try {
					unify_raise(ctx, ec.etype, t, ec.epos);
				} catch (e: Error(Unify(_))(_)) {
					raise(Not_found);
				};
				try {
					unify_enum_field(en, List.map(function _: mk_mono([]), en.e_params), ef, t);
				} catch (e: Unify_error(l)) {
					error(error_msg(Unify(l)), p);
				};
				mk_con_pat(CEnum(en, ef), [], t, p);
			case TConst(c) | TCast({ eexpr = TConst(c) }, None):
				try {
					unify_raise(ctx, ec.etype, t, ec.epos);
				} catch (e: Error(Unify(_))(_)) {
					raise(Not_found);
				};
				unify(ctx, ec.etype, t, p);
				mk_con_pat(CConst(c), [], t, p);
			case TTypeExpr(mt): mk_type_pat(ctx, mt, t, p);
			case _: raise(Not_found);
			};
		};
		function loop(pctx, e, t) return {
			var p = pos(e);
			switch (fst(e)) {
			case ECheckType(e, CTPath({ tpackage = ::(haxe, ::(macro, [])); tname = Expr })): var old = pctx.pc_reify;
				pctx.pc_reify = True;
				var e = loop(pctx, e, t);
				pctx.pc_reify = old;
				e;
			case EParenthesis(e): loop(pctx, e, t);
			case ECast(e1, None): loop(pctx, e1, t);
			case EConst(Ident(null)): if (!(matches_null(ctx, t))) {
					error( ^ ("Null-patterns are only allowed on nullable types [found ", ^ (s_type(t), "]")), p);
				} else {
					[];
				};
				mk_con_pat(CConst(TNull), [], t, p);
			case EConst(Ident(false | true) | Int(_) | String(_) | Float(_) = c): var e = Codegen.type_constant(ctx.com, c, p);
				unify(ctx, e.etype, t, p);
				var c = switch (e.eexpr) {
				case TConst(c): c;
				case _: assert False;
				};
				mk_con_pat(CConst(c), [], t, p);
			case EMeta((Meta.Macro, [], _), (ECall(e1, args), _)): var Tuple(path, field, args) = Codegen.get_macro_path(ctx, e1, args,
						p);
				switch (ctx.g.do_macro(ctx, MExpr, path, field, args, p)) {
				case Some(e): loop(pctx, e, t);
				case None: error("Macro failure", p);
				};
			case EField(_): var e = type_expr(ctx, e, WithType(t));
				var e = switch (Optimizer.make_constant_expression(ctx, concat_strings = True, e)) {
				case Some(e): e;
				case None: e;
				};
				switch (e.eexpr) {
				case TConst(c) | TCast({ eexpr = TConst(c) }, None): mk_con_pat(CConst(c), [], t, p);
				case TTypeExpr(mt): mk_type_pat(ctx, mt, t, p);
				case TField(_, FStatic(_, cf)) if (is_value_type(cf.cf_type)): ignore(follow(cf.cf_type));
					switch (cf.cf_expr) {
					case Some(e): try {
							check_texpr_pattern(e, t, p);
						} catch (e: Not_found) {
							mk_con_pat(CExpr(e), [], cf.cf_type, p);
						};
					case None: mk_con_pat(CExpr(e), [], cf.cf_type, p);
					};
				case TField(_, FEnum(en, ef)): try {
						unify_enum_field(en, List.map(function _: mk_mono([]), en.e_params), ef, t);
					} catch (e: Unify_error(l)) {
						error(error_msg(Unify(l)), p);
					};
					mk_con_pat(CEnum(en, ef), [], t, p);
				case _: error("Constant expression expected", p);
				};
			case ECall(ec, el): var ec = type_expr(ctx, ec, WithType(t));
				switch (follow(ec.etype)) {
				case TEnum(en, pl) | TFun(_, TEnum(en, pl)): var ef = switch (ec.eexpr) {
					case TField(_, FEnum(_, f)): f;
					case _: error( ^ ("Expected constructor for enum ", s_type_path(en.e_path)), p);
					};
					var monos = List.map(function _: mk_mono([]), ef.ef_params);
					var Tuple(tl, r) = switch (apply_params(en.e_params, pl, apply_params(ef.ef_params, monos, ef.ef_type))) {
					case TFun(args, r): unify(ctx, r, t, p);
						(new Tuple(List.map(function (n, _, t): t, args), r));
					case _: error("No arguments expected", p);
					};
					function loop2(i, el, tl) return {
						switch ((new Tuple(el, tl))) {
						case (::((EConst(Ident(_)), pany), []), ::(t, tl)): var pat = mk_pat(PAny, t_dynamic, pany);
							ExtList.List.make(+(List.length(tl), 1), pat);
						case (::(e, el), ::(t, tl)): var pat = loop(pctx, e, t);
							::(pat, loop2(+(i, 1), el, tl));
						case (::(e, _), []): error("Too many arguments", pos(e));
						case ([], ::(_, _)): error("Not enough arguments", p);
						case ([], []): [];
						};
					};
					var el = loop2(0, el, tl);
					List.iter2(function m: function (_, t):
					switch (follow(m)) {
				case TMono(_): Type.unify(m, t);
					case _: [];
					}, monos, ef.ef_params);
					pctx.pc_is_complex = True;
					mk_con_pat(CEnum(en, ef), el, r, p);
				case _: perror(p);
				};
			case EConst(Ident(_)): switch (get_tuple_params(t)) {
				case Some(tl): var pl = List.map(function (_, _, t): mk_any(t, p), tl);
					mk_pat(PTuple(Array.of_list(pl)), t_dynamic, p);
				case None: mk_any(t, p);
				};
			case EConst(Ident(s)): try {
					var ec = switch (follow(t)) {
					case TEnum(en, pl): var ef = try {
							PMap.find(s, en.e_constrs);
						} catch (e: Not_found) {
							error(string_error(s, en.e_names, ^ ("Expected constructor for enum ", s_type_path(en.e_path))), p);
						};
						switch (ef.ef_type) {
						case TFun(args, _): var msg = Printf.sprintf("Enum constructor %s.%s requires parameters %s", s_type_path(en.e_path),
														  ef.ef_name, String.concat(", ", List.map(function (n, _, t): ^ (n, ^ (":", s_type(t))), args)));
							error(msg, p);
						case _: [];
						};
						var et = mk(TTypeExpr(TEnumDecl(en)), TAnon({ () with a_fields = PMap.empty;
									a_status = ref(EnumStatics(en))
																	}), p);
						mk(TField(et, FEnum(en, ef)), apply_params(en.e_params, pl, ef.ef_type), p);
					case TAbstract({ a_impl = Some(c) } = a, _) if (Meta.has(Meta.Enum, a.a_meta)): var cf = PMap.find(s, c.cl_statics);
						Type.unify(follow(cf.cf_type), t);
						var e = switch (cf.cf_expr) {
						case Some({ eexpr = TConst(c) | TCast({ eexpr = TConst(c) }, None) } = e): e;
						case _: raise(Not_found);
						};
						e;
					case _: var old = ctx.untyped;
						ctx.untyped = True;
						var e = try {
							type_expr(ctx, e, WithType(t));
						} catch (e: _) {
							ctx.untyped = old;
							raise(Not_found);
						};
						ctx.untyped = old;
						e;
					};
					check_texpr_pattern(ec, t, p);
				} catch (e: Not_found) {
					switch (get_tuple_params(t)) {
					case Some(tl): var s = String.concat(",", List.map(function (_, _, t): s_type(t), tl));
						error( ^ ("Pattern should be tuple [", ^ (s, "]")), p);
					case None: if ( && (!(is_lower_ident(s)), <>(s0, '`'))) {
							error("Capture variables must be lower-case", p);
						} else {
							[];
						};
						var v = mk_var(pctx, s, t, p);
						mk_pat(PVar(v, p), v.v_type, p);
					};
				};
			case EObjectDecl(fl): 	function is_matchable(cf) return {
					switch (cf.cf_kind) {
					case Method(_): False;
					case _: True;
					};
				};
				function is_valid_field_name(fields, co, n, p) return {
					try {
						var cf = PMap.find(n, fields);
						switch (co) {
						case Some(c) if (!(Typer.can_access(ctx, c, cf, False))): error( ^ ("Cannot match against private field ", n), p);
						case _: [];
						};
					} catch (e: Not_found) {
						error( ^ (s_type(t), ^ (" has no field ", ^ (n, " that can be matched against"))), p);
					};
				};
				pctx.pc_is_complex = True;
				function loop_fields(fields) return {
					var Tuple(sl, pl, i) = PMap.foldi(function n: function cf: function (sl, pl, i):
					if (!(is_matchable(cf))) {
					(new Tuple(sl, pl, i));
					} else {
						var pat = try {
							if ( && (pctx.pc_reify, = (cf.cf_name, "pos"))) {
								raise(Not_found);
							} else {
								[];
							};
							loop(pctx, List.assoc(cf.cf_name, fl), cf.cf_type);
						} catch (e: Not_found) {
							mk_any(cf.cf_type, p);
						};
						(new Tuple(::((new Tuple(n, cf)), sl), ::(pat, pl), +(i, 1)));
					}, fields, (new Tuple([], [], 0)));
					mk_con_pat(CFields(i, sl), pl, t, p);
				};
				var fields = switch (follow(t)) {
				case TAnon({ a_fields = fields }): fields;
				case TInst(c, tl): var fields = ref(PMap.empty);
					function loop(c, tl) return {
						switch (c.cl_super) {
						case Some(csup, tlsup): loop(csup, List.map(apply_params(c.cl_params, tl), tlsup));
						case None: [];
						};
						PMap.iter(function n: function cf: fields.val = PMap.add(n, { (cf) with cf_type = apply_params(c.cl_params, tl, monomorphs(cf.cf_params, cf.cf_type)) }, fields.val), c.cl_fields);
					};
					loop(c, tl);
					fields.val;
				case TAbstract({ a_impl = Some(c) } = a, tl): var fields = List.fold_left(function acc: function cf:
					if (Meta.has(Meta.Impl, cf.cf_meta)) {
					PMap.add(cf.cf_name, cf, acc);
					} else {
						acc;
					}, PMap.empty, c.cl_ordered_statics);
					PMap.map(function cf: {
								 (cf) with cf_type = apply_params(a.a_params, tl, monomorphs(cf.cf_params, cf.cf_type))
							 }, fields);
				case _: error( ^ (s_type(t), " cannot be matched against a structure"), p);
				};
				List.iter(function (n, (_, p)): is_valid_field_name(fields, None, n, p), fl);
				loop_fields(fields);
			case EArrayDecl([]): mk_con_pat(CArray(0), [], t, p);
			case EArrayDecl(el): pctx.pc_is_complex = True;
				switch (follow(t)) {
				case TInst({ cl_path = ([], Array) }, ::(t2, [])) | TDynamic(_) = t2: var pl = ExtList.List.mapi(
								function i: function e: loop(pctx, e, t2), el);
					mk_con_pat(CArray(List.length(el)), pl, t, p);
				case TFun(tl, tr) if (==(tr, fake_tuple_type)): var pl = try {
						List.map2(function e: function (_, _, t): loop(pctx, e, t), el, tl);
					} catch (e: Invalid_argument(_)) {
						error( ^ ("Invalid number of arguments: expected ", ^ (string_of_int(List.length(tl)), ^ (", found ",
								  string_of_int(List.length(el))))), p);
					};
					mk_pat(PTuple(Array.of_list(pl)), t, p);
				case _: error( ^ (s_type(t), " should be Array"), p);
				};
			case EBinop(OpAssign, (EConst(Ident(s)), p2), e1): var v = mk_var(pctx, s, t, p);
				var pat1 = loop(pctx, e1, t);
				mk_pat(PBind((new Tuple(v, p)), pat1), t, p2);
			case EBinop(OpOr, (EBinop(OpOr, e1, e2), p2), e3): loop(pctx, (new Tuple(EBinop(OpOr, e1, (new Tuple(EBinop(OpOr, e2, e3),
						p2))), p)), t);
			case EBinop(OpOr, e1, e2): var old = pctx.pc_locals;
				var pat1 = loop(pctx, e1, t);
				switch (pat1.p_def) {
				case PAny | PVar(_): display_error(ctx, "This pattern is unused", pos(e2));
					pat1;
				case _: var pctx2 = { () with pc_sub_vars = Some(pctx.pc_locals);
										  pc_locals = old;
										  pc_reify = pctx.pc_reify;
										  pc_is_complex = pctx.pc_is_complex
										};
					var pat2 = loop(pctx2, e2, t);
					pctx.pc_is_complex = pctx2.pc_is_complex;
					PMap.iter(function s: function (_, p): if (!(PMap.mem(s, pctx2.pc_locals))) {
					verror(s, p);
					} else {
						[];
					}, pctx.pc_locals);
					mk_pat(POr(pat1, pat2), pat2.p_type, punion(pat1.p_pos, pat2.p_pos));
				};
			case _: raise(Unrecognized_pattern(e));
			};
		};
		var pctx = { () with pc_locals = PMap.empty;
					 pc_sub_vars = None;
					 pc_reify = False;
					 pc_is_complex = False
				   };
		var x = loop(pctx, e, t);
		(new Tuple(x, pctx.pc_locals, pctx.pc_is_complex));
	};

	public static function get_pattern_locals(ctx, e, t) return {
		try {
			var Tuple(_, locals, _) = to_pattern(ctx, e, t);
			PMap.foldi(function n: function v: function acc: PMap.add(n, v, acc), locals, PMap.empty);
		} catch (e: Unrecognized_pattern(_)) {
			PMap.empty;
		};
	};

	public static function expr_eq(e1, e2) return {
		|| ( == (e1, e2), switch ((new Tuple(e1.eexpr, e2.eexpr))) {
	case (TConst(ct1), TConst(ct2)): = (ct1, ct2);
		case (TField(_, FStatic(c1, cf1)), TField(_, FStatic(c2, cf2))): && ( == (c1, c2), = (cf1.cf_name, cf2.cf_name));
		case _: False;
		});
	};

	public static function unify_con(con1, con2) return {
		switch ((new Tuple(con1.c_def, con2.c_def))) {
		case (CExpr(e1), CExpr(e2)): expr_eq(e1, e2);
		case (CConst(c1), CConst(c2)): = (c1, c2);
		case (CEnum(e1, ef1), CEnum(e2, ef2)): && ( == (e1, e2), = (ef1.ef_name, ef2.ef_name));
		case (CFields(i1, fl1), CFields(i2, fl2)): try {
				List.iter(function (s, _):
				if (!(List.mem_assoc(s, fl1))) {
				raise(Not_found);
				} else {
					[];
				}, fl2);
				True;
			} catch (e: Not_found) {
				False;
			};
		case (CType(mt1), CType(mt2)): = (t_path(mt1), t_path(mt2));
		case (CArray(a1), CArray(a2)): == (a1, a2);
		case (CAny, CAny): True;
		case _: False;
		};
	};

	public static function array_tl(arr) return {
		Array.sub(arr, 1, -(Array.length(arr), 1));
	};

	public static function spec(mctx, con, pmat) return {
		var a = arity(con);
		var r = DynArray.create([]);
		function add(pv, out) return {
			DynArray.add(r, (new Tuple(pv, out)));
		};
		function loop2(pv, out) return {
			switch (pv0.p_def) {
			case PCon(c2, pl) if (unify_con(c2, con)): add(Array.append(Array.of_list(pl), array_tl(pv)), out);
			case PCon(c2, pl): [];
			case PAny | PVar(_): add(Array.append(Array.make(a, mk_any(pv0.p_type, pv0.p_pos)), array_tl(pv)), out);
			case PBind(_, pat): loop2(Array.append([pat], array_tl(pv)), out);
			case PTuple(tl): loop2(tl, out);
			case POr(_): assert False;
			};
		};
		function loop(pmat) return {
			switch (pmat) {
			case ::((pv, out), pl): loop2(pv, out);
				loop(pl);
			case []: [];
			};
		};
		loop(pmat);
		DynArray.to_list(r);
	};

	public static function default(mctx, pmat) return {
		var r = DynArray.create([]);
		function add(pv, out) return {
			DynArray.add(r, (new Tuple(pv, out)));
		};
		function loop2(pv, out) return {
			switch (pv0.p_def) {
			case PCon(_): [];
			case PAny | PVar(_): add(array_tl(pv), out);
			case PBind(_, pat): loop2(Array.append([pat], array_tl(pv)), out);
			case PTuple(tl): loop2(tl, out);
			case POr(_): assert False;
			};
		};
		function loop(pmat) return {
			switch (pmat) {
			case ::((pv, out), pl): loop2(pv, out);
				loop(pl);
			case []: [];
			};
		};
		loop(pmat);
		DynArray.to_list(r);
	};

	public static function pick_column(pmat) return {
		function loop(i, pv) return {
			if ( = (Array.length(pv), 0)) {
				-1;
			} else {
				switch (pv0.p_def) {
				case PVar(_) | PAny: loop(+(i, 1), array_tl(pv));
				case PTuple(pl): loop(i, pl);
				case _: i;
				};
			};
		};
		loop(0, fst(List.hd(pmat)));
	};

	public static function swap_pmat_columns(i, pmat) return {
		List.map(function (pv, out): var pv = switch (pv) {
	case {
			p_def = PTuple(pt)
			}: pt;
		case _: pv;
		};
		var tmp = pvi;
				  Array.set(pv, i, pv0);
				  Array.set(pv, 0, tmp);
				  (new Tuple(pv, out)), pmat);
	};

	public static function swap_columns(i, rowlist(a)) return {
		switch (row) {
		case ::(rh, rt): 	function loop(count, acc, col) return {
				switch (col) {
				case []: acc;
				case ::(ch, cl) if (=(i, count)): @(::(ch, List.rev(acc)), @(::(rh, []), cl));
				case ::(ch, cl): loop(+(count, 1), ::(ch, acc), cl);
				};
			};
			loop(1, [], rt);
		case _: [];
		} : list(a);
	};

	public static function expand_or(mctx, pmatpat_matrix) return {
		function loop(pat) return {
			switch (pat.p_def) {
			case POr(pat1, pat2): var pat1 = loop(pat1);
				var pat2 = loop(pat2);
				@(pat1, pat2);
			case PBind(v, pat1): var pat1 = loop(pat1);
				List.map(function pat1: {
					(pat) with p_def = PBind(v, pat1)
				}, pat1);
			case PTuple(pl): var pat1 = loop(pl0);
				List.map(function pat1: var a1 = Array.copy(pl);
						 a10 = pat1;
				{
					(pat) with p_def = PTuple(a1)
				}, pat1);
			case _: ::(pat, []);
			};
		};
		function loop2(pmat) return {
			switch (pmat) {
			case ::((pv, out), pmat): var pat = loop(pv0);
				var pat' = ExtList.List.mapi(function i: function pat: var a1 = Array.copy(pv);
				a10 = pat;
				(new Tuple(a1, out)), pat);
				@(pat', loop2(pmat));
			case []: [];
				};
		};
		loop2(pmat);
	};

	public static function column_sigma(mctx, st, pmat) return {
		var acc = ref([]);
		var bindings = ref([]);
		var unguarded = Hashtbl.create(0);
		function add(c, g) return {
			if (!(List.exists(function c2: unify_con(c2, c), acc.val))) {
				acc.val = ::(c, acc.val);
			} else {
				[];
			};
			if (!(g)) {
				Hashtbl.replace(unguarded, c.c_def, True);
			} else {
				[];
			};
		};
		function bind_st(out, st, v) return {
			if (!(List.exists(function ((v2, p), _): == (v2.v_id, fst(v).v_id), bindings.val))) {
				bindings.val = ::((new Tuple(v, st)), bindings.val);
			} else {
				[];
			};
		};
		function loop(pmat) return {
			switch (pmat) {
			case ::((pv, out), pr): 	function loop2(out) return {
				case PCon(c, _): add(c, <>(get_guard(mctx, out.o_id), None));
				case PVar(v): bind_st(out, st, v);
				case PBind(v, pat): bind_st(out, st, v);
					loop2(out, pat.p_def);
				case PAny: [];
				case PTuple(tl): loop2(out, tl0.p_def);
				case POr(_): assert False;
				};
				loop2(out, pv0.p_def);
				loop(pr);
			case []: [];
			};
		};
		loop(pmat);
		(new Tuple(List.rev_map(function con: (new Tuple(con, !(Hashtbl.mem(unguarded, con.c_def)))), acc.val), bindings.val));
	};

	public static function all_ctors(mctx, t) return {
		var h = ref(PMap.empty);
		if (is_explicit_null(t)) {
			h.val = PMap.add(CConst(TNull), Ast.null_pos, h.val);
		} else {
			[];
		};
		switch (follow(t)) {
		case TAbstract({ a_path = ([], Bool) }, _): h.val = PMap.add(CConst(TBool(True)), Ast.null_pos, h.val);
			h.val = PMap.add(CConst(TBool(False)), Ast.null_pos, h.val);
			(new Tuple(h, RunTimeFinite));
		case TAbstract({ a_impl = Some(c) } = a, pl) if (Meta.has(Meta.Enum, a.a_meta)): List.iter(function cf: ignore(follow(
						cf.cf_type));
			if (Meta.has(Meta.Impl, cf.cf_meta)) {
			switch (cf.cf_expr) {
				case Some({ eexpr = TConst(c) | TCast({ eexpr = TConst(c) }, None) }): h.val = PMap.add(CConst(c), cf.cf_pos, h.val);
				case _: [];
				};
			} else {
				[];
			}, c.cl_ordered_statics);
			(new Tuple(h, CompileTimeFinite));
		case TAbstract(a, pl) if (!(Meta.has(Meta.CoreType, a.a_meta))): all_ctors(mctx, Abstract.get_underlying_type(a, pl));
		case TInst({ cl_path = ([], String) }, _) | TInst({ cl_path = ([], Array) }, _): (new Tuple(h, Infinite));
		case TEnum(en, pl): PMap.iter(function _: function ef: var tc = monomorphs(mctx.ctx.type_params, t);
			try {
				unify_enum_field(en, pl, ef, tc);
				h.val = PMap.add(CEnum(en, ef), ef.ef_pos, h.val);
			} catch (e: Unify_error(_)) {
				[];
			}, en.e_constrs);
			(new Tuple(h, RunTimeFinite));
		case TAnon(a): (new Tuple(h, CompileTimeFinite));
		case TInst(_, _): (new Tuple(h, CompileTimeFinite));
		case _: (new Tuple(h, Infinite));
		};
	};

	public static function collapse_pattern(pl) return {
		switch (pl) {
		case ::(pat, []): pat;
		case ::(pat, pl): var pat2 = collapse_pattern(pl);
			mk_pat(POr(pat, pat2), pat.p_type, punion(pat.p_pos, pat2.p_pos));
		case []: assert False;
		};
	};

	public static function bind_remaining(out, pv, stl) return {
		function loop(stl, pv) return {
			if ( = (Array.length(pv), 0)) {
				[];
			} else {
				switch ((new Tuple(stl, pv0.p_def))) {
				case (::(st, stl), PAny): loop(stl, array_tl(pv));
				case (::(st, stl), PVar(v)): ::((new Tuple(v, st)), loop(stl, array_tl(pv)));
				case (stl, PTuple(pl)): loop(stl, pl);
				case (::(_, _), _): loop(stl, array_tl(pv));
				case ([], _): [];
				};
			};
		};
		loop(stl, pv);
	};

	public static function get_cache(mctx, dt) return {
		switch (dt) {
		case Goto(_): dt;
		case _: try {
				Goto(Hashtbl.find(mctx.dt_cache, dt));
			} catch (e: Not_found) {
				Hashtbl.replace(mctx.dt_cache, dt, mctx.dt_count);
				mctx.dt_count = +(mctx.dt_count, 1);
				DynArray.add(mctx.dt_lut, dt);
				dt;
			};
		};
	};

	public static function compile(mctx, stl, pmat, toplevel) return {
		function guard(id, dt1, dt2) return {
			get_cache(mctx, Guard(id, dt1, dt2));
		};
		function expr(id) return {
			get_cache(mctx, Expr(id));
		};
		function bind(bl, dt) return {
			get_cache(mctx, Bind(bl, dt));
		};
		function switch (st, cl) return {
			get_cache(mctx, Switch(st, cl));
		};
		get_cache(mctx, switch (pmat) {
	case []: switch (stl) {
			case ::(st, stl): var Tuple(all, inf) = all_ctors(mctx, st.st_type);
				var pl = PMap.foldi(function cd: function p: function acc: ::(mk_con_pat(cd, [], t_dynamic, p), acc), all.val, []);
				switch ((new Tuple(pl, inf))) {
				case (_, Infinite) | ([], _): raise(Not_exhaustive(any, st));
				case _: raise(Not_exhaustive(collapse_pattern(pl), st));
				};
			case _: raise(Exit);
			};
		case ::(( {
							  p_def = PTuple(pt)
						  }, out), pl): compile(mctx, stl, ::((new Tuple(pt, out)), pl), toplevel);
		case ::((pv, out), pl): var i = pick_column(pmat);
			if ( = (i, -1)) {
				out.o_num_paths = +(out.o_num_paths, 1);
				var bl = bind_remaining(out, pv, stl);
				var dt = switch (get_guard(mctx, out.o_id)) {
				case None: expr(out.o_id);
				case Some(_): var dt = switch ((new Tuple(pl, mctx.need_val))) {
					case ([], False): None;
					case _: Some(compile(mctx, stl, pl, False));
					};
					guard(out.o_id, expr(out.o_id), dt);
				};
				if ( = (bl, [])) {
					dt;
				} else {
					bind(bl, dt);
				};
			} else {
				if ( > (i, 0)) {
					var pmat = swap_pmat_columns(i, pmat);
					var stls = swap_columns(i, stl);
					compile(mctx, stls, pmat, toplevel);
				} else {
					var Tuple(st_head, st_tail) = switch (stl) {
					case ::(st, stl): (new Tuple(st, stl));
					case _: assert False;
					};
					var pmat = expand_or(mctx, pmat);
					var Tuple(sigma, bl) = column_sigma(mctx, st_head, pmat);
					var Tuple(all, inf) = all_ctors(mctx, pv0.p_type);
					var cases = List.map(function (c, g):
					if (!(g)) {
					all.val = PMap.remove(c.c_def, all.val);
					} else {
						[];
					};
					var spec = spec(mctx, c, pmat);
							   var hsubs = mk_subs(st_head, c);
							   var subs = @(hsubs, st_tail);
							   var dt = compile(mctx, subs, spec, False);
							   (new Tuple(c, dt)), sigma);
					var def = default(mctx, pmat);
					var dt = switch ((new Tuple(def, cases))) {
					case _ if (&&(=(inf, RunTimeFinite), PMap.is_empty(all.val))):
						switch (st_head, cases);
					case ([], _) if (&&(=(inf, CompileTimeFinite), PMap.is_empty(all.val))):
						switch (st_head, cases);
					case ([], _) if (&&(=(inf, Infinite), &&(!(mctx.need_val), toplevel))): mctx.is_exhaustive = False;
						switch (st_head, cases);
					case ([], _) if (=(inf, Infinite)): raise(Not_exhaustive(any, st_head));
					case ([], _): var pl = PMap.foldi(function cd: function p: function acc: ::(mk_con_pat(cd, [], t_dynamic, p), acc),
														  all.val, []);
						if ( && (toplevel, switch (pl) {
						case ::( {
											 p_def = PCon({ c_def = CConst(TNull) }, _)
										 }, []): True;
							case _: False;
							})) {
							switch (st_head, cases);
						} else {
							raise(Not_exhaustive(collapse_pattern(pl), st_head));
						};
					case (def, []): compile(mctx, st_tail, def, False);
					case (def, _): var cdef = mk_con(CAny, t_dynamic, st_head.st_pos);
						var def = try {
							compile(mctx, st_tail, def, False);
						} catch (e: Exit) {
							raise(Not_exhaustive(any, st_head));
						};
						var cases = @(cases, ::((new Tuple(cdef, def)), []));
						switch (st_head, cases);
					};
					if ( = (bl, [])) {
						dt;
					} else {
						bind(bl, dt);
					};
				};
			};
		});
	};

	public static function collapse_case(el) return {
		switch (el) {
		case ::(e, []): e;
		case ::(e, el): var e2 = collapse_case(el);
			(new Tuple(EBinop(OpOr, e, e2), punion(pos(e), pos(e2))));
		case []: assert False;
		};
	};

	public static function mk_const(ctx, p) return {
	case TString(s): mk(TConst(TString(s)), ctx.com.basic.tstring, p);
	case TInt(i): mk(TConst(TInt(i)), ctx.com.basic.tint, p);
	case TFloat(f): mk(TConst(TFloat(f)), ctx.com.basic.tfloat, p);
	case TBool(b): mk(TConst(TBool(b)), ctx.com.basic.tbool, p);
	case TNull: mk(TConst(TNull), ctx.com.basic.tnull(mk_mono([])), p);
	case _: error("Unsupported constant", p);
	};

	public static function convert_st(ctx, st) return {
		switch (st.st_def) {
		case SVar(v): mk(TLocal(v), v.v_type, st.st_pos);
		case SField(sts, cf): var e = convert_st(ctx, sts);
			Typer.acc_get(ctx, Typer.type_field(ctx, e, cf.cf_name, st.st_pos, Typer.MGet), st.st_pos);
		case SArray(sts, i): mk(TArray(convert_st(ctx, sts), mk_const(ctx, st.st_pos, TInt(Int32.of_int(i)))), st.st_type,
			st.st_pos);
		case STuple(st, _, _): convert_st(ctx, st);
		case SEnum(sts, ef, i): mk(TEnumParameter(convert_st(ctx, sts), ef, i), st.st_type, st.st_pos);
		};
	};

	public static function convert_con(ctx, con) return {
		switch (con.c_def) {
		case CConst(c): mk_const(ctx, con.c_pos, c);
		case CType(mt): mk(TTypeExpr(mt), t_dynamic, con.c_pos);
		case CExpr(e): e;
		case CEnum(e, ef) if (Meta.has(Meta.FakeEnum, e.e_meta)): var e_mt = type_module_type_ref.val(ctx, TEnumDecl(e), None,
			con.c_pos);
			mk(TField(e_mt, FEnum(e, ef)), con.c_type, con.c_pos);
		case CEnum(e, ef): mk_const(ctx, con.c_pos, TInt(Int32.of_int(ef.ef_index)));
		case CArray(i): mk_const(ctx, con.c_pos, TInt(Int32.of_int(i)));
		case CAny | CFields(_): assert False;
		};
	};

	public static function convert_switch(mctx, st, cases, loop) return {
		var ctx = mctx.ctx;
		var e_st = convert_st(ctx, st);
		var p = e_st.epos;
		function mk_index_call([]) return {
			var ttype = switch (follow(Typeload.load_instance(ctx, {
			() with tpackage = ::("std", []);
				tname = "Type";
				tparams = [];
				tsub = None
			}, p, True))) {
			case TInst(c, _): c;
			case t: assert False;
			};
			var cf = PMap.find("enumIndex", ttype.cl_statics);
			var ec = type_module_type_ref.val(ctx, TClassDecl(ttype), None, p);
			var ef = mk(TField(ec, FStatic(ttype, cf)), tfun(::(e_st.etype, []), ctx.t.tint), p);
			var e = make_call(ctx, ef, ::(e_st, []), ctx.t.tint, p);
			e;
		};
		function wrap_exhaustive(e) return {
			if (mctx.is_exhaustive) {
				mk(TMeta((new Tuple(Meta.Exhaustive, [], e.epos)), e), e.etype, e.epos);
			} else {
				e;
			};
		};
		var e = switch (follow(st.st_type)) {
		case TEnum(en, _) if (Meta.has(Meta.FakeEnum, en.e_meta)): wrap_exhaustive(e_st);
		case TEnum(_): wrap_exhaustive(mk_index_call([]));
		case TAbstract(a, pl) if (switch (Abstract.get_underlying_type(a, pl)) {
				case TEnum(_): True;
					case _: False;
					}): wrap_exhaustive(mk_index_call([]));
		case TInst({ cl_path = ([], Array) }, _) = t: mk(TField(e_st, quick_field(t, "length")), ctx.t.tint, p);
		case TAbstract(a, _) if (Meta.has(Meta.Enum, a.a_meta)): wrap_exhaustive(e_st);
		case TAbstract({ a_path = ([], Bool) }, _): wrap_exhaustive(e_st);
		case _: 	function loop(cases) return {
				switch (cases) {
				case []: e_st;
				case ::((con, _), cases): switch (con.c_def) {
					case CEnum(_): mk_index_call([]);
					case CArray(_): mk(TField(e_st, FDynamic("length")), ctx.t.tint, p);
					case _: loop(cases);
					};
				};
			};
			loop(cases);
		};
		var null = ref(None);
		var def = ref(None);
		var cases = List.filter(function (con, dt):
		switch (con.c_def) {
	case CConst(TNull): null.val = Some(loop(dt));
			False;
		case CAny: def.val = Some(loop(dt));
			False;
		case _: True;
		}, cases);
		var dt = switch (cases) {
		case ::(( {
							  c_def = CFields(_)
						  }, dt), []): loop(dt);
		case _: DTSwitch(e, List.map(function (c, dt): (new Tuple(convert_con(ctx, c), loop(dt))), cases), def.val);
		};
		switch (null.val) {
		case None if (&&(is_explicit_null(st.st_type), ||(<>(def.val, None), !(mctx.need_val)))): var econd = mk(TBinop(OpNotEq,
					e_st, mk(TConst(TNull), st.st_type, p)), ctx.t.tbool, p);
			DTGuard(econd, dt, def.val);
		case None: dt;
		case Some(dt_null): var t = switch (ctx.t.tnull(ctx.t.tint)) {
			case TType(t, _): TType(t, ::(st.st_type, []));
			case t: t;
			};
			var e_null = mk(TConst(TNull), t, p);
			var econd = mk(TBinop(OpEq, e_st, e_null), ctx.t.tbool, p);
			DTGuard(econd, dt_null, Some(dt));
		};
	};

	public static function transform_extractors(eval, cases, p) return {
		var efail = (new Tuple(EThrow(EConst(Ident("false")), p), p));
		var cfail = (new Tuple(::((new Tuple(EConst(Ident("_")), p)), []), None, Some(efail)));
		var has_extractor = ref(False);
		function loop(cases) return {
			switch (cases) {
			case ::((epat, eg, e), cases): var ex = ref([]);
				var exc = ref(0);
				function find_ex(in_or, e) return {
					switch (fst(e)) {
					case EBinop(OpArrow, _, _) if (in_or): error("Extractors in or patterns are not allowed", pos(e));
					case EBinop(OpArrow, e1, e2): var ec = (new Tuple(EConst(Ident( ^ ("__ex", string_of_int(exc.val)))), snd(e)));
						function map_left(e) return {
							switch (fst(e)) {
							case EConst(Ident(_)): ec;
							case _: Ast.map_expr(map_left, e);
							};
						};
						var ecall = map_left(e1);
						ex.val = ::((new Tuple(ecall, e2)), ex.val);
						incr(exc);
						has_extractor.val = True;
						ec;
					case EBinop(OpOr, e1, e2): var e1 = find_ex(True, e1);
						var e2 = find_ex(True, e2);
						(new Tuple(EBinop(OpOr, e1, e2), pos(e)));
					case _: Ast.map_expr(find_ex(in_or), e);
					};
				};
				var p = switch (e) {
				case None: p;
				case Some(e): pos(e);
				};
				var epat = switch (epat) {
				case ::(epat, []): ::(find_ex(False, epat), []);
				case _: List.map(find_ex(True), epat);
				};
				var cases = loop(cases);
				if ( = (exc.val, 0)) {
					::((new Tuple(epat, eg, e)), cases);
				} else {
					var esubjects = (new Tuple(EArrayDecl(List.map(fst, ex.val)), p));
					var case1 = (new Tuple(::((new Tuple(EArrayDecl(List.map(snd, ex.val)), p)), []), eg, e));
					var cases2 = switch (cases) {
					case []: ::(case1, []);
					case ::((::((EConst(Ident(_)), _), []), _, e), []): ::(case1, ::((new Tuple(::((new Tuple(EConst(Ident("_")), p)), []), None, e)), []));
					case _: ::(case1, ::((new Tuple(::((new Tuple(EConst(Ident("_")), p)), []), None, Some(ESwitch(eval, cases, None), p))), []));
					};
					var eswitch = (new Tuple(ESwitch(esubjects, cases2, None), p));
				var case = (new Tuple(epat, None, Some(eswitch)));
					switch (epat) {
					case ::((EConst(Ident(_)), _), []): ::(case, ::(cfail, []));
					case _: ::(case, cases);
					};
				};
			case []: [];
			};
		};
		var cases = loop(cases);
		(new Tuple(cases, has_extractor.val));
	};

	public static var extractor_depth = ref(0);

	public static function match_expr(ctx, e, cases, def, with_type, p) return {
		var Tuple(need_val, with_type, tmono) = switch (with_type) {
		case NoValue: (new Tuple(False, NoValue, None));
		case WithType(t) | WithTypeResume(t) if (switch (follow(t)) {
				case TMono(_): True;
					case _: False;
					}): (new Tuple(True, Value, Some(with_type)));
		case t: (new Tuple(True, t, None));
		};
		var cases = switch ((new Tuple(cases, def))) {
		case ([], None): [];
		case (cases, Some(def)): var p = switch (def) {
			case None: p;
			case Some(_, p): p;
			};
			@(cases, ::((new Tuple(::((new Tuple(EConst(Ident("_")), p)), []), None, def)), []));
		case _: cases;
		};
		var Tuple(cases, has_extractor) = transform_extractors(e, cases, p);
		var array_match = ref(False);
		var evals = switch (fst(e)) {
		case EArrayDecl(el) | EParenthesis(EArrayDecl(el), _) if (switch (el) {
				case ::((EFor(_) | EWhile(_), _), []): False;
					case _: True;
					}): array_match.val = True;
			List.map(function e: type_expr(ctx, e, Value), el);
		case _: var e = type_expr(ctx, e, Value);
			switch (follow(e.etype)) {
			case TEnum(en, _) if (switch (en.e_path) {
					case (::(neko | php | flash | cpp, []), XmlType): True;
						case _: False;
						}): raise(Exit);
			case TAbstract({ a_path = ([], Int | Float | Bool) }, _) | TInst({ cl_path = ([], String) }, _) if (Common.defined(ctx.com, Common.Define.NoPatternMatching))
						: raise(Exit);
			case _: [];
			};
			::(e, []);
		};
		var var_inits = ref([]);
		var save = save_locals(ctx);
		var a = List.length(evals);
		var stl = ExtList.List.mapi(function i: function e: 	function loop(e) return {
			switch (e.eexpr) {
			case TParenthesis(e) | TMeta(_, e): loop(e);
			case TLocal(v): mk_st(SVar(v), e.etype, e.epos);
			case _: var v = gen_local(ctx, e.etype);
				var_inits.val = ::((new Tuple(v, Some(e))), var_inits.val);
				ctx.locals = PMap.add(v.v_name, v, ctx.locals);
				mk_st(SVar(v), e.etype, e.epos);
			};
		};
		var st = loop(e);
		if ( = (a, 1)) {
		st;
	} else {
		mk_st(STuple(st, i, a), st.st_type, st.st_pos);
		}, evals);
		var tl = List.map(function st: st.st_type, stl);
		var mctx = {
			() with ctx = ctx;
			need_val = need_val;
			outcomes = [];
			toplevel_or = False;
			dt_lut = DynArray.create([]);
			dt_cache = Hashtbl.create(0);
			dt_count = 0;
			has_extractor = has_extractor;
			expr_map = PMap.empty;
			is_exhaustive = True
		};
		var cases = List.map(function (el, eg, e): List.iter(function e:
		switch (fst(e)) {
	case EBinop(OpOr, _, _): mctx.toplevel_or = True;
		case _: [];
		}, el);
		switch (el) {
	case []: var p = switch (e) {
			case None: p;
			case Some(e): pos(e);
			};
			error("case without a pattern is not allowed", p);
		case _: (new Tuple(collapse_case(el), eg, e));
		}, cases);
		var is_complex = ref(False);
		if (mctx.has_extractor) {
			incr(extractor_depth);
		} else {
			[];
		};
		function add_pattern_locals(Tuple(pat, locals, complex)) return {
			PMap.iter(function n: function (v, p): ctx.locals = PMap.add(n, v, ctx.locals), locals);
			if (complex) {
				is_complex.val = True;
			} else {
				[];
			};
			pat;
		};
		var pl = ExtList.List.mapi(function i: function (ep, eg, e): var save = save_locals(ctx);
		var Tuple(pl, restore, with_type) = try {
			var monos = List.map(function _: mk_mono([]), ctx.type_params);
			var t = switch (tl) {
			case ::(t, []) if (!(array_match.val)): t;
			case tl: tfun(tl, fake_tuple_type);
			};
			var t = apply_params(ctx.type_params, monos, t);
			var pl = ::(add_pattern_locals(to_pattern(ctx, ep, t)), []);
			var old_ret = ctx.ret;
			ctx.ret = apply_params(ctx.type_params, monos, ctx.ret);
			var restore = PMap.fold(function v: function acc: var t = v.v_type;
									v.v_type = apply_params(ctx.type_params, monos, v.v_type);
									::(function []: v.v_type = t, acc), ctx.locals, ::(function []: ctx.ret = old_ret, []));
			List.iter2(function m: function (_, t):
			switch (follow(m)) {
		case TMono(_): Type.unify(m, t);
			case _: [];
			}, monos, ctx.type_params);
			(new Tuple(pl, restore, switch (with_type) {
		case WithType(t): WithType(apply_params(ctx.type_params, monos, t));
			case WithTypeResume(t): WithTypeResume(apply_params(ctx.type_params, monos, t));
			case _: with_type;
			}));
		} catch (e: Unrecognized_pattern(e)(p)) {
			error("Case expression must be a constant value or a pattern, not an arbitrary expression", p);
		};
		var is_catch_all = switch (pl) {
	case ::( {
						 p_def = PAny | PVar(_)
					 }, []): True;
		case _: False;
		};
		var e = switch (e) {
	case None: mk(TBlock([]), ctx.com.basic.tvoid, pos(ep));
		case Some(e): type_expr(ctx, e, with_type);
		};
		var e = switch (with_type) {
	case WithType(t): Codegen.AbstractCast.cast_or_unify(ctx, t, e, e.epos);
		case WithTypeResume(t): try {
				Codegen.AbstractCast.cast_or_unify_raise(ctx, t, e, e.epos);
			} catch (e: Error(Unify(l))(p)) {
				raise(Typer.WithTypeError(l, p));
			};
		case _: e;
		};
		var eg = switch (eg) {
	case None: None;
	case Some(e): var eg = type_expr(ctx, e, WithType(ctx.com.basic.tbool));
			unify(ctx, eg.etype, ctx.com.basic.tbool, eg.epos);
			Some(eg);
		};
		List.iter(function f: f([]), restore);
		save([]);
		var out = mk_out(mctx, i, e, eg, is_catch_all, pos(ep));
				  (new Tuple(Array.of_list(pl), out)), cases);
		function check_unused([]) return {
			function unused(p) return {
				display_error(ctx, "This pattern is unused", p);
				var old_error = ctx.on_error;
				ctx.on_error = function ctx: function s: function p: ctx.on_error = old_error;
				raise(Exit);
				function check_expr(e, p) return {
					try {
						switch (fst(e)) {
						case EConst(Ident(null | true | false)): [];
						case EConst(Ident(_)): ignore(type_expr(ctx, e, Value));
							display_error(ctx, "Case expression must be a constant value or a pattern, not an arbitrary expression", pos(e));
						case _: [];
						};
					} catch (e: Exit) {
						[];
					};
				};
				function loop(prev, cl) return {
					switch (cl) {
					case ::((_, Some(_), _), cl): loop(prev, cl);
					case ::(((e, p2), _, _), cl): if ( >= (p2.pmin, p.pmin)) {
							check_expr(prev, p);
						} else {
							loop((new Tuple(e, p2)), cl);
						};
					case []: check_expr(prev, p);
					};
				};
				switch (cases) {
				case ::((e, _, _), cl): loop(e, cl);
				case []: assert False;
				};
				ctx.on_error = old_error;
			};
			var had_catch_all = ref(False);
			List.iter(function out:
			if ( && (out.o_catch_all, !(had_catch_all.val))) {
			had_catch_all.val = True;
		} else {
			if ( = (out.o_num_paths, 0)) {
					unused(out.o_pos);
					if (mctx.toplevel_or) {
						switch (evals) {
						case ::( {
								etype = t
							}, []) if (switch (follow(t)) {
							case TAbstract({ a_path = ([], Int) }, []): True;
								case _: False;
								}): display_error(ctx, "Note: Int | Int is an or-pattern now", p);
						case _: [];
						};
					} else {
						[];
					};
				} else {
					[];
				};
			}, List.rev(mctx.outcomes));
		};
		var dt = try {
			compile(mctx, stl, pl, True);
		} catch (e: Not_exhaustive(pat)(st)) {
			function s_st_r(top, pre, st, v) return {
				switch (st.st_def) {
				case SVar(v1): if (!(pre)) {
						v;
					} else {
						try {
							var e = switch (List.assoc(v1, var_inits.val)) {
							case Some(e): e;
							case None: assert False;
							};
							^ (Type.s_expr_pretty("", Type.s_type(print_context([])), e), v);
						} catch (e: Not_found) {
							^ (v1.v_name, v);
						};
					};
				case STuple(st, i, a): var r = -(-(a, i), 1);
					Printf.sprintf("[%s]", st_args(i, r, s_st_r(top, False, st, v)));
				case SArray(st, i): s_st_r(False, True, st, Printf.sprintf("[%i]%s", i, if (top) {
					^ (" = ", v);
					} else {
						v;
					}));
				case SField({ st_def = SVar(v1) }, cf) if (=(v1.v_name0, '`')): ^ (cf.cf_name, if (top) {
					^ (" = ", v);
					} else {
						v;
					});
				case SField(st, cf): s_st_r(False, True, st, Printf.sprintf(".%s%s", cf.cf_name, if (top) {
					^ (" = ", v);
					} else {
						v;
					}));
				case SEnum(st, ef, i): var len = switch (follow(ef.ef_type)) {
					case TFun(args, _): List.length(args);
					case _: 0;
					};
					s_st_r(False, False, st, Printf.sprintf("%s[%s]", ef.ef_name, st_args(i, -(-(len, 1), i), v)));
				};
			};
			var pat = switch (follow(st.st_type)) {
			case TAbstract({ a_impl = Some(cl) } = a, _) if (Meta.has(Meta.Enum, a.a_meta)): 	function s_pat(pat) return {
					switch (pat.p_def) {
					case PCon({ c_def = CConst(c) }, []) if (<>(c, TNull)): var cf = List.find(function cf:
						switch (cf.cf_expr) {
					case Some({ eexpr = TConst(c2) | TCast({ eexpr = TConst(c2) }, None) }): = (c, c2);
						case _: False;
						}, cl.cl_ordered_statics);
						cf.cf_name;
					case PVar(v, _): v.v_name;
					case PCon(c, []): s_con(c);
					case PCon(c, pl): ^ (s_con(c), ^ ("[", ^ (String.concat(",", List.map(s_pat, pl)), "]")));
					case POr(pat1, pat2): ^ (s_pat(pat1), ^ (" | ", s_pat(pat2)));
					case PAny: "_";
					case PBind((v, _), pat): ^ (v.v_name, ^ ("=", s_pat(pat)));
					case PTuple(pl): ^ ("[", ^ (String.concat(" ", Array.to_list(Array.map(s_pat, pl))), "]"));
					};
				};
				s_pat(pat);
			case _: s_pat(pat);
			};
			var msg = ^ ("Unmatched patterns: ", s_st_r(True, False, st, pat));
			if ( > (extractor_depth.val, 0)) {
				display_error(ctx, msg, st.st_pos);
				error("Note: Patterns with extractors may require a default pattern", st.st_pos);
			} else {
				error(msg, st.st_pos);
			};
		};
		save([]);
		if ( = (extractor_depth.val, 0)) {
			check_unused([]);
		} else {
			[];
		};
		if (mctx.has_extractor) {
			decr(extractor_depth);
		} else {
			[];
		};
		var t = if (!(need_val)) {
			mk_mono([]);
		} else {
			switch (with_type) {
			case WithType(t) | WithTypeResume(t): t;
			case _: try {
					Typer.unify_min_raise(ctx, List.rev_map(function (_, out): get_expr(mctx, out.o_id), List.rev(pl)));
				} catch (e: Error(Unify(l))(p)) {
					error(error_msg(Unify(l)), p);
				};
			};
		};
		switch (tmono) {
		case None: [];
		case Some(WithType(t2)): unify(ctx, t2, t, p);
		case Some(WithTypeResume(t2)): try {
				unify_raise(ctx, t2, t, p);
			} catch (e: Error(Unify(l))(p)) {
				raise(Typer.WithTypeError(l, p));
			};
		case _: assert False;
		};
		var usage = Array.make(DynArray.length(mctx.dt_lut), 0);
		var first = switch (dt) {
		case Goto(i): i;
		case _: Hashtbl.find(mctx.dt_cache, dt);
		};
		Array.set(usage, first, 2);
		function loop(dt) return {
			switch (dt) {
			case Goto(i): Array.set(usage, i, +(Array.get(usage, i), 1));
			case Switch(st, cl): List.iter(function (_, dt): loop(dt), cl);
			case Bind(bl, dt): loop(dt);
			case Expr(e): [];
			case Guard(e, dt1, dt2): loop(dt1);
				switch (dt2) {
				case None: [];
				case Some(dt): loop(dt);
				};
			};
		};
		DynArray.iter(loop, mctx.dt_lut);
		var map = Array.make(DynArray.length(mctx.dt_lut), 0);
		var lut = DynArray.create([]);
		function loop(i, c) return {
			if ( < (c, DynArray.length(mctx.dt_lut))) {
				var i' = if (>(usagec, 1)) {
				DynArray.add(lut, DynArray.get(mctx.dt_lut, c));
				+(i, 1);
			} else {
				i;
			};
				Array.set(map, c, i);
				loop(i', +(c, 1));
			} else {
				[];
			};
		};
		loop(0, 0);
		function loop(dt) return {
			switch (dt) {
			case Goto(i): if ( > (usagei, 1)) {
					DTGoto(mapi);
				} else {
					loop(DynArray.get(mctx.dt_lut, i));
				};
			case Switch(st, cl): convert_switch(mctx, st, cl, loop);
			case Bind(bl, dt): DTBind(List.map(function (v, st): (new Tuple(v, convert_st(ctx, st))), bl), loop(dt));
			case Expr(id): DTExpr(get_expr(mctx, id));
			case Guard(id, dt1, dt2): DTGuard(switch (get_guard(mctx, id)) {
			case Some(e): e;
				case None: assert False;
				}, loop(dt1), switch (dt2) {
			case None: None;
			case Some(dt): Some(loop(dt));
				});
			};
		};
		var lut = DynArray.map(loop, lut);
		{
			() with dt_first = mapfirst;
			dt_dt_lookup = DynArray.to_array(lut);
			dt_type = t;
			dt_var_init = List.rev(var_inits.val);
			dt_is_complex = is_complex.val
		};
	};

	public static function __init__() {
		match_expr_ref.val = match_expr;
		get_pattern_locals_ref.val = get_pattern_locals;
	}
}
;
