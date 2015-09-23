import Ast;
import Common;
import Type;

typedef Dce = {
	com : Context,
	full : Bool,
	std_dirs : List<String>,
	debug : Bool,
	follow_expr : Dce -> Texpr -> Unit,
	curclass : Tclass,
	added_fields : List<Tuple<Tclass, Tclass_field, Bool>>,
	marked_fields : List<Tclass_field>,
	marked_maybe_fields : List<Tclass_field>,
	t_stack : List<T>,
	ts_stack : List<T>,
	features : Hashtbl<String, Tuple<Tclass, Tclass_field, Bool>>
};

class Dce {
	public static function super_forces_keep(c) return {
		|| (Meta.has(Meta.KeepSub, c.cl_meta), switch (c.cl_super) {
	case Some(csup, _): super_forces_keep(csup);
		case _: False;
		});
	};

	public static function is_std_file(dce, file) return {
		List.exists(ExtString.String.starts_with(file), dce.std_dirs);
	};

	public static function keep_whole_class(dce, c) return {
		|| (Meta.has(Meta.Keep, c.cl_meta), || (!( || (dce.full, || (is_std_file(dce, c.cl_module.m_extra.m_file), has_meta(Meta.Dce, c.cl_meta)))), || (super_forces_keep(c), switch (c) {
	case {
			cl_path = ([], Math | Array)
			} if (=(dce.com.platform, Js)): False;
		case {
				cl_extern = True
			} | { cl_path = (::(flash, ::(_Boot, [])), RealBoot) }: True;
		case {
				cl_path = ([], String)
			} | { cl_path = ([], Array) }: !( = (dce.com.platform, Js));
		case _: False;
		})));
	};

	public static function keep_whole_enum(dce, en) return {
		|| (Meta.has(Meta.Keep, en.e_meta), !( || (dce.full, || (is_std_file(dce, en.e_module.m_extra.m_file), has_meta(Meta.Dce, en.e_meta)))));
	};

	public static function keep_field(dce, cf) return {
		|| (Meta.has(Meta.Keep, cf.cf_meta), || (Meta.has(Meta.Used, cf.cf_meta), || ( = (cf.cf_name, "__init__"), is_extern_field(cf))));
	};

	public static function check_feature(dce, s) return {
		try {
			var l = Hashtbl.find(dce.features, s);
			List.iter(function (c, cf, stat): mark_field(dce, c, cf, stat), l);
			Hashtbl.remove(dce.features, s);
		} catch (e: Not_found) {
			[];
		};
	};

	public static function check_and_add_feature(dce, s) return {
		check_feature(dce, s);
		Common.add_feature(dce.com, s);
	};

	public static function mark_field(dce, c, cf, stat) return {
		function add(cf) return {
			if (!(Meta.has(Meta.Used, cf.cf_meta))) {
				cf.cf_meta = ::((new Tuple(Meta.Used, [], cf.cf_pos)), cf.cf_meta);
				dce.added_fields = ::((new Tuple(c, cf, stat)), dce.added_fields);
				dce.marked_fields = ::(cf, dce.marked_fields);
				check_feature(dce, Printf.sprintf("%s.%s", s_type_path(c.cl_path), cf.cf_name));
			} else {
				[];
			};
		};
		if ( = (cf.cf_name, "new")) {
			function loop(c) return {
				switch (c.cl_super) {
				case None: [];
				case Some(csup, _): switch (csup.cl_constructor) {
					case None: [];
					case Some(cf): add(cf);
					};
					loop(csup);
				};
			};
			loop(c);
		} else {
			[];
		};
		if (!(PMap.mem(cf.cf_name, if (stat) {
			c.cl_statics;
		} else {
			c.cl_fields;
		}))) {
			switch (c.cl_super) {
			case None: add(cf);
			case Some(c, _): mark_field(dce, c, cf, stat);
			};
		} else {
			add(cf);
		};
	};

	public static function update_marked_class_fields(dce, c) return {
		List.iter(function cf:
		if (Meta.has(Meta.MaybeUsed, cf.cf_meta)) {
		mark_field(dce, c, cf, True);
		} else {
			[];
		}, c.cl_ordered_statics);
		List.iter(function cf:
		if (Meta.has(Meta.MaybeUsed, cf.cf_meta)) {
		mark_field(dce, c, cf, False);
		} else {
			[];
		}, c.cl_ordered_fields);
		switch (c.cl_init) {
		case None: [];
		case Some(init): dce.follow_expr(dce, init);
		};
		List.iter(function (c, _): mark_class(dce, c), c.cl_implements);
		switch (c.cl_super) {
		case None: [];
		case Some(csup, pl): mark_class(dce, csup);
		};
	};

	public static function mark_class(dce, c) return {
		if (!(Meta.has(Meta.Used, c.cl_meta))) {
			c.cl_meta = ::((new Tuple(Meta.Used, [], c.cl_pos)), c.cl_meta);
			check_feature(dce, Printf.sprintf("%s.*", s_type_path(c.cl_path)));
			update_marked_class_fields(dce, c);
		} else {
			[];
		};
	};

	public static function mark_enum(dce, e) return {
		if (!(Meta.has(Meta.Used, e.e_meta))) {
			e.e_meta = ::((new Tuple(Meta.Used, [], e.e_pos)), e.e_meta);
			check_and_add_feature(dce, "has_enum");
			check_feature(dce, Printf.sprintf("%s.*", s_type_path(e.e_path)));
			PMap.iter(function _: function ef: mark_t(dce, ef.ef_pos, ef.ef_type), e.e_constrs);
		} else {
			[];
		};
	};

	public static function mark_abstract(dce, a) return {
		if (!(Meta.has(Meta.Used, a.a_meta))) {
			check_feature(dce, Printf.sprintf("%s.*", s_type_path(a.a_path)));
			a.a_meta = ::((new Tuple(Meta.Used, [], a.a_pos)), a.a_meta);
		} else {
			[];
		};
	};

	public static function mark_t(dce, p, t) return {
		if (!(List.exists(function t2: Type.fast_eq(t, t2), dce.t_stack))) {
			dce.t_stack = ::(t, dce.t_stack);
			switch (follow(t)) {
			case TInst({ cl_kind = KTypeParameter(tl) } = c, pl):
				if (!(Meta.has(Meta.Used, c.cl_meta))) {
					c.cl_meta = ::((new Tuple(Meta.Used, [], c.cl_pos)), c.cl_meta);
					List.iter(mark_t(dce, p), tl);
				} else {
					[];
				};
				List.iter(mark_t(dce, p), pl);
			case TInst(c, pl): mark_class(dce, c);
				List.iter(mark_t(dce, p), pl);
			case TFun(args, ret): List.iter(function (_, _, t): mark_t(dce, p, t), args);
				mark_t(dce, p, ret);
			case TEnum(e, pl): mark_enum(dce, e);
				List.iter(mark_t(dce, p), pl);
			case TAbstract(a, pl) if (Meta.has(Meta.MultiType, a.a_meta)):
				try {
					mark_t(dce, p, snd(Codegen.AbstractCast.find_multitype_specialization(dce.com, a, pl, p)));
				} catch (e: Typecore.Error(_)) {
					[];
				};
			case TAbstract(a, pl): mark_abstract(dce, a);
				List.iter(mark_t(dce, p), pl);
				if (!(Meta.has(Meta.CoreType, a.a_meta))) {
					mark_t(dce, p, Abstract.get_underlying_type(a, pl));
				} else {
					[];
				};
			case TLazy(_) | TDynamic(_) | TType(_) | TAnon(_) | TMono(_): [];
			};
			dce.t_stack = List.tl(dce.t_stack);
		} else {
			[];
		};
	};

	public static function mark_mt(dce, mt) return {
		switch (mt) {
		case TClassDecl(c): mark_class(dce, c);
		case TEnumDecl(e): mark_enum(dce, e);
		case TAbstractDecl(a): if (!(Meta.has(Meta.ValueUsed, a.a_meta))) {
				a.a_meta = ::((new Tuple(Meta.ValueUsed, [], a.a_pos)), a.a_meta);
			} else {
				[];
			};
			mark_abstract(dce, a);
		case TTypeDecl(_): [];
		};
	};

	public static function mark_dependent_fields(dce, csup, n, stat) return {
		List.iter(function mt:
		switch (mt) {
	case TClassDecl(c) if (is_parent(csup, c)): 	function loop(c) return {
				try {
					var cf = PMap.find(n, if (stat) {
					c.cl_statics;
				} else {
					c.cl_fields;
				});
					if ( || (Meta.has(Meta.Used, c.cl_meta), && (csup.cl_interface, csup.cl_extern))) {
						mark_field(dce, c, cf, stat);
					} else {
						if (!(Meta.has(Meta.MaybeUsed, cf.cf_meta))) {
							cf.cf_meta = ::((new Tuple(Meta.MaybeUsed, [], cf.cf_pos)), cf.cf_meta);
							dce.marked_maybe_fields = ::(cf, dce.marked_maybe_fields);
						} else {
							[];
						};
					};
				} catch (e: Not_found) {
					switch (c.cl_super) {
					case None: [];
					case Some(csup, _): loop(csup);
					};
				};
			};
			loop(c);
		case _: [];
		}, dce.com.types);
	};

	public static function opt(f, e) return {
		switch (e) {
		case None: [];
		case Some(e): f(e);
		};
	};

	public static function to_string(dce, t) return {
		switch (t) {
		case TInst(c, tl): field(dce, c, "toString", False);
		case TType(tt, tl): if (!(List.exists(function t2: Type.fast_eq(t, t2), dce.ts_stack))) {
				dce.ts_stack = ::(t, dce.ts_stack);
				to_string(dce, apply_params(tt.t_params, tl, tt.t_type));
			} else {
				[];
			};
		case TAbstract({ a_impl = Some(c) } = a, tl):
			if (Meta.has(Meta.CoreType, a.a_meta)) {
				field(dce, c, "toString", False);
			} else {
				to_string(dce, Abstract.get_underlying_type(a, tl));
			};
		case TMono(r): switch (r.val) {
			case Some(t): to_string(dce, t);
			case _: [];
			};
		case TLazy(f): to_string(dce, f.val([]));
		case TDynamic(t): if ( == (t, t_dynamic)) {
				[];
			} else {
				to_string(dce, t);
			};
		case TEnum(_) | TFun(_) | TAnon(_) | TAbstract({ a_impl = None }, _): [];
		};
	};

	public static function field(dce, c, n, stat) return {
		function find_field(n) return {
			if ( = (n, "new")) {
				switch (c.cl_constructor) {
				case None: raise(Not_found);
				case Some(cf): cf;
				};
			} else {
				PMap.find(n, if (stat) {
				c.cl_statics;
			} else {
				c.cl_fields;
			});
			};
		};
		try {
			var cf = find_field(n);
			mark_field(dce, c, cf, stat);
		} catch (e: Not_found) {
			try {
				if (c.cl_interface) {
					function loop(cl) return {
						switch (cl) {
						case []: raise(Not_found);
						case ::((c, _), cl): try {
								field(dce, c, n, stat);
							} catch (e: Not_found) {
								loop(cl);
							};
						};
					};
					loop(c.cl_implements);
				} else {
					switch (c.cl_super) {
					case Some(csup, _): field(dce, csup, n, stat);
					case None: raise(Not_found);
					};
				};
			} catch (e: Not_found) {
				try {
					switch (c.cl_kind) {
					case KTypeParameter(tl): 	function loop(tl) return {
							switch (tl) {
							case []: raise(Not_found);
							case ::(TInst(c, _), cl): try {
									field(dce, c, n, stat);
								} catch (e: Not_found) {
									loop(cl);
								};
							case ::(t, tl): loop(tl);
							};
						};
						loop(tl);
					case _: raise(Not_found);
					};
				} catch (e: Not_found) {
					if (dce.debug) {
						prerr_endline( ^ ("[DCE] Field ", ^ (n, ^ (" not found on ", s_type_path(c.cl_path)))));
					} else {
						[];
					};
				};
			};
		};
	};

	public static function mark_directly_used_class(c) return {
		if (!(Meta.has(Meta.DirectlyUsed, c.cl_meta))) {
			c.cl_meta = ::((new Tuple(Meta.DirectlyUsed, [], c.cl_pos)), c.cl_meta);
		} else {
			[];
		};
	};

	public static function mark_directly_used_enum(e) return {
		if (!(Meta.has(Meta.DirectlyUsed, e.e_meta))) {
			e.e_meta = ::((new Tuple(Meta.DirectlyUsed, [], e.e_pos)), e.e_meta);
		} else {
			[];
		};
	};

	public static function mark_directly_used_mt(mt) return {
		switch (mt) {
		case TClassDecl(c): mark_directly_used_class(c);
		case TEnumDecl(e): mark_directly_used_enum(e);
		case _: [];
		};
	};

	public static function check_dynamic_write(dce, fa) return {
		var n = field_name(fa);
		check_and_add_feature(dce, "dynamic_write");
		check_and_add_feature(dce, ^ ("dynamic_write.", n));
	};

	public static function check_anon_optional_write(dce, fa) return {
		var n = field_name(fa);
		check_and_add_feature(dce, "anon_optional_write");
		check_and_add_feature(dce, ^ ("anon_optional_write.", n));
	};

	public static function check_anon_write(dce, fa) return {
		var n = field_name(fa);
		check_and_add_feature(dce, "anon_write");
		check_and_add_feature(dce, ^ ("anon_write.", n));
	};

	public static function is_array(t) return {
		switch (follow(t)) {
		case TAbstract(a, tl) if (!(Meta.has(Meta.CoreType, a.a_meta))): is_array(Abstract.get_underlying_type(a, tl));
		case TInst({ cl_path = ([], Array) }, _): True;
		case _: False;
		};
	};

	public static function is_dynamic(t) return {
		switch (follow(t)) {
		case TAbstract(a, tl) if (!(Meta.has(Meta.CoreType, a.a_meta))): is_dynamic(Abstract.get_underlying_type(a, tl));
		case TDynamic(_): True;
		case _: False;
		};
	};

	public static function is_string(t) return {
		switch (follow(t)) {
		case TAbstract(a, tl) if (!(Meta.has(Meta.CoreType, a.a_meta))): is_string(Abstract.get_underlying_type(a, tl));
		case TInst({ cl_path = ([], String) }, _): True;
		case _: False;
		};
	};

	public static function is_const_string(e) return {
		switch (e.eexpr) {
		case TConst(TString(_)): True;
		case _: False;
		};
	};

	public static function expr(dce, e) return {
		mark_t(dce, e.epos, e.etype);
		switch (e.eexpr) {
		case TNew(c, pl, el): mark_class(dce, c);
			mark_directly_used_class(c);
			field(dce, c, "new", False);
			List.iter(expr(dce), el);
			List.iter(mark_t(dce, e.epos), pl);
		case TVar(v, e1): opt(expr(dce), e1);
			mark_t(dce, e.epos, v.v_type);
		case TCast(e, Some(mt)): check_feature(dce, "typed_cast");
			mark_mt(dce, mt);
			mark_directly_used_mt(mt);
			expr(dce, e);
		case TObjectDecl(vl): check_and_add_feature(dce, "has_anon");
			List.iter(function (_, e): expr(dce, e), vl);
		case TTypeExpr(mt): mark_mt(dce, mt);
			mark_directly_used_mt(mt);
		case TTry(e, vl): expr(dce, e);
			List.iter(function (v, e):
			if ( != (v.v_type, t_dynamic)) {
			check_feature(dce, "typed_catch");
			} else {
				[];
			};
			expr(dce, e);
			mark_t(dce, e.epos, v.v_type), vl);
		case TCall({ eexpr = TLocal({ v_name = `trace }) }, ::(p, ::({ eexpr = TObjectDecl(v) }, []))): check_and_add_feature(dce,
					"has_anon_trace");
			List.iter(function (_, e): expr(dce, e), v);
			expr(dce, p);
		case TCall({ eexpr = TLocal({ v_name = __define_feature__ }) }, ::({ eexpr = TConst(TString(ft)) }, ::(e, []))):
			Hashtbl.replace(dce.curclass.cl_module.m_extra.m_features, ft, True);
			check_feature(dce, ft);
			expr(dce, e);
		case TCall({ eexpr = TField({ eexpr = TTypeExpr(TClassDecl({ cl_path = (::(haxe, []), Log) } = c)) }, FStatic(_, { cf_name = trace })) } = ef, ::(e2, el) = args) | TCall({ eexpr = TField({ eexpr = TTypeExpr(TClassDecl({ cl_path = ([], Std) } = c)) }, FStatic(_, { cf_name = string })) } = ef, ::(e2, el) = args)
				: mark_class(dce, c);
			to_string(dce, e2.etype);
			switch (el) {
			case ::( {
								 eexpr = TObjectDecl(fl)
							 }, []):
				try {
					switch (List.assoc("customParams", fl)) {
					case {
							eexpr = TArrayDecl(el)
						}: List.iter(function e: to_string(dce, e.etype), el);
					case _: [];
					};
				} catch (e: Not_found) {
					[];
				};
			case _: [];
			};
			expr(dce, ef);
			List.iter(expr(dce), args);
		case TCall({ eexpr = TConst(TSuper) } = e, el): mark_t(dce, e.epos, e.etype);
			List.iter(expr(dce), el);
		case TBinop(OpAdd, e1, e2) if (||(is_dynamic(e1.etype), is_dynamic(e2.etype))): check_and_add_feature(dce, "add_dynamic");
			expr(dce, e1);
			expr(dce, e2);
		case TBinop(OpAdd | OpAssignOp(OpAdd), e1, e2) if (&&(||(is_string(e1.etype), is_string(e2.etype)), !(&&(is_const_string(e1), is_const_string(e2)))))
					: check_and_add_feature(dce, "unsafe_string_concat");
			expr(dce, e1);
			expr(dce, e2);
		case TArray({ etype = TDynamic(t) } = e1, e2) if (==(t, t_dynamic)): check_and_add_feature(dce, "dynamic_array_read");
			expr(dce, e1);
			expr(dce, e2);
		case TBinop(OpAssign | OpAssignOp(_), { eexpr = TArray({ etype = TDynamic(t) }, _) } = e1, e2) if (==(t, t_dynamic)):
			check_and_add_feature(dce, "dynamic_array_write");
			expr(dce, e1);
			expr(dce, e2);
		case TArray({ etype = t } = e1, e2) if (is_array(t)): check_and_add_feature(dce, "array_read");
			expr(dce, e1);
			expr(dce, e2);
		case TBinop(OpAssign | OpAssignOp(_), { eexpr = TArray({ etype = t }, _) } = e1, e2) if (is_array(t)):
			check_and_add_feature(dce, "array_write");
			expr(dce, e1);
			expr(dce, e2);
		case TBinop(OpAssign, { eexpr = TField(_, FDynamic(_) = fa) } = e1, e2): check_dynamic_write(dce, fa);
			expr(dce, e1);
			expr(dce, e2);
		case TBinop(OpAssign, { eexpr = TField(_, FAnon(cf) = fa) } = e1, e2):
			if (Meta.has(Meta.Optional, cf.cf_meta)) {
				check_anon_optional_write(dce, fa);
			} else {
				check_anon_write(dce, fa);
			};
			expr(dce, e1);
			expr(dce, e2);
		case TBinop(OpAssignOp(op), { eexpr = TField(_, FDynamic(_) = fa) } = e1, e2): check_dynamic_write(dce, fa);
			expr(dce, e1);
			expr(dce, e2);
		case TBinop(OpAssignOp(op), { eexpr = TField(_, FAnon(cf) = fa) } = e1, e2):
			if (Meta.has(Meta.Optional, cf.cf_meta)) {
				check_anon_optional_write(dce, fa);
			} else {
				check_anon_write(dce, fa);
			};
			expr(dce, e1);
			expr(dce, e2);
		case TBinop(OpEq, { etype = t1 } = e1, { etype = t2 } = e2) if (||(is_dynamic(t1), is_dynamic(t2))): check_and_add_feature(
				dce, "dynamic_binop_==");
			expr(dce, e1);
			expr(dce, e2);
		case TBinop(OpEq, { etype = t1 } = e1, { etype = t2 } = e2) if (||(is_dynamic(t1), is_dynamic(t2))): check_and_add_feature(
				dce, "dynamic_binop_!=");
			expr(dce, e1);
			expr(dce, e2);
		case TBinop(OpMod, e1, e2): check_and_add_feature(dce, "binop_%");
			expr(dce, e1);
			expr(dce, e2);
		case TBinop(OpUShr | OpAssignOp(OpUShr), e1, e2): check_and_add_feature(dce, "binop_> > >");
			expr(dce, e1);
			expr(dce, e2);
		case TField(e, fa): switch (fa) {
			case FStatic(c, cf): mark_class(dce, c);
				mark_field(dce, c, cf, True);
			case FInstance(c, _, cf): mark_class(dce, c);
				mark_field(dce, c, cf, False);
			case _: var n = field_name(fa);
				switch (fa) {
				case FAnon(cf): if (Meta.has(Meta.Optional, cf.cf_meta)) {
						check_and_add_feature(dce, "anon_optional_read");
						check_and_add_feature(dce, ^ ("anon_optional_read.", n));
					} else {
						check_and_add_feature(dce, "anon_read");
						check_and_add_feature(dce, ^ ("anon_read.", n));
					};
				case FDynamic(_): check_and_add_feature(dce, "dynamic_read");
					check_and_add_feature(dce, ^ ("dynamic_read.", n));
				case _: [];
				};
				switch (follow(e.etype)) {
				case TInst(c, _): mark_class(dce, c);
					field(dce, c, n, False);
				case TAnon(a): switch (a.a_status.val) {
					case Statics(c): mark_class(dce, c);
						field(dce, c, n, True);
					case _: [];
					};
				case _: [];
				};
			};
			expr(dce, e);
		case TThrow(e): check_and_add_feature(dce, "has_throw");
			to_string(dce, e.etype);
			expr(dce, e);
		case _: Type.iter(expr(dce), e);
		};
	};

	public static function fix_accessors(com) return {
		List.iter(function mt:
		switch (mt) {
	case TClassDecl(c): 	function has_accessor(c, n, stat) return {
				|| (PMap.mem(n, if (stat) {
				c.cl_statics;
			} else {
				c.cl_fields;
			}), switch (c.cl_super) {
			case Some(csup, _): has_accessor(csup, n, stat);
				case None: False;
				});
			};
			function check_prop(stat, cf) return {
				switch (cf.cf_kind) {
				case Var({ v_read = AccCall; v_write = a }): var s = ^ ("get_", cf.cf_name);
					cf.cf_kind = Var({ () with v_read = if (has_accessor(c, s, stat)) {
					AccCall;
				} else {
					AccNever;
				};
				v_write = a
								 });
				case _: [];
				};
				switch (cf.cf_kind) {
				case Var({ v_write = AccCall; v_read = a }): var s = ^ ("set_", cf.cf_name);
					cf.cf_kind = Var({ () with v_write = if (has_accessor(c, s, stat)) {
					AccCall;
				} else {
					AccNever;
				};
				v_read = a
								 });
				case _: [];
				};
			};
			List.iter(check_prop(True), c.cl_ordered_statics);
			List.iter(check_prop(False), c.cl_ordered_fields);
		case _: [];
		}, com.types);
	};

	public static function run(com, main, full) return {
		var dce = {
			() with com = com;
			full = full;
			std_dirs = if (full) {
				[];
			} else {
				List.map(Common.unique_full_path, com.std_path);
			};
			debug = Common.defined(com, Define.DceDebug);
			added_fields = [];
			follow_expr = expr;
			marked_fields = [];
			marked_maybe_fields = [];
			t_stack = [];
			ts_stack = [];
			features = Hashtbl.create(0);
			curclass = null_class
		};
		switch (main) {
		case Some({ eexpr = TCall({ eexpr = TField(e, FStatic(c, cf)) }, _) }): cf.cf_meta = ::((new Tuple(Meta.Keep, [],
					cf.cf_pos)), cf.cf_meta);
		case _: [];
		};
		List.iter(function m: List.iter(function (s, v):
		if (Hashtbl.mem(dce.features, s)) {
		Hashtbl.replace(dce.features, s, ::(v, Hashtbl.find(dce.features, s)));
		} else {
			Hashtbl.add(dce.features, s, ::(v, []));
		}, m.m_extra.m_if_feature), com.modules);
		List.iter(function t:
		switch (t) {
	case TClassDecl(c): var keep_class = && (keep_whole_class(dce, c), || (!(c.cl_extern), c.cl_interface));
			function loop(stat, cf) return {
				if ( || (keep_class, keep_field(dce, cf))) {
					mark_field(dce, c, cf, stat);
				} else {
					[];
				};
			};
			List.iter(loop(True), c.cl_ordered_statics);
			List.iter(loop(False), c.cl_ordered_fields);
			switch (c.cl_constructor) {
			case Some(cf): loop(False, cf);
			case None: [];
			};
			switch (c.cl_init) {
			case Some(e) if (||(keep_class, Meta.has(Meta.KeepInit, c.cl_meta))): var cf = mk_field("__init__", e.etype, e.epos);
				cf.cf_expr = Some(e);
				loop(True, cf);
			case _: [];
			};
		case TEnumDecl(en) if (keep_whole_enum(dce, en)): mark_enum(dce, en);
		case _: [];
		}, com.types);
		if (dce.debug) {
			List.iter(function (c, cf, _):
			switch (cf.cf_expr) {
		case None: [];
			case Some(_): print_endline( ^ ("[DCE] Entry point: ", ^ (s_type_path(c.cl_path), ^ (".", cf.cf_name))));
			}, dce.added_fields);
		} else {
			[];
		};
		function loop([]) return {
			switch (dce.added_fields) {
			case []: [];
			case cfl: dce.added_fields = [];
				List.iter(function (c, cf, stat): mark_dependent_fields(dce, c, cf.cf_name, stat), cfl);
				List.iter(function (c, cf, stat):
				if (!(is_extern_field(cf))) {
				mark_class(dce, c);
				} else {
					[];
				};
				mark_field(dce, c, cf, stat);
				mark_t(dce, cf.cf_pos, cf.cf_type), cfl);
				List.iter(function (c, cf, _): dce.curclass = c;
						  opt(expr(dce), cf.cf_expr);
						  List.iter(function cf:
				if (<>(cf.cf_expr, None)) {
				opt(expr(dce), cf.cf_expr);
				} else {
					[];
				}, cf.cf_overloads), cfl);
				loop([]);
			};
		};
		loop([]);
		function loop(acc, types) return {
			switch (types) {
			case ::(TClassDecl(c) = mt, l) if (keep_whole_class(dce, c)): loop(::(mt, acc), l);
			case ::(TClassDecl(c) = mt, l): 	function check_property(cf, stat) return {
					function add_accessor_metadata(cf) return {
						if (!(Meta.has(Meta.Accessor, cf.cf_meta))) {
							cf.cf_meta = ::((new Tuple(Meta.Accessor, [], c.cl_pos)), cf.cf_meta);
						} else {
							[];
						};
					};
					switch (cf.cf_kind) {
					case Var({ v_read = AccCall }):
						try {
							add_accessor_metadata(PMap.find( ^ ("get_", cf.cf_name), if (stat) {
							c.cl_statics;
						} else {
							c.cl_fields;
						}));
						} catch (e: Not_found) {
							[];
						};
					case _: [];
					};
					switch (cf.cf_kind) {
					case Var({ v_write = AccCall }):
						try {
							add_accessor_metadata(PMap.find( ^ ("set_", cf.cf_name), if (stat) {
							c.cl_statics;
						} else {
							c.cl_fields;
						}));
						} catch (e: Not_found) {
							[];
						};
					case _: [];
					};
				};
				c.cl_meta = ::((new Tuple(Meta.Keep, [], c.cl_pos)), c.cl_meta);
				c.cl_ordered_statics = List.filter(function cf: var b = keep_field(dce, cf);
				if (!(b)) {
				if (dce.debug) {
						print_endline( ^ ("[DCE] Removed field ", ^ (s_type_path(c.cl_path), ^ (".", cf.cf_name))));
					} else {
						[];
					};
					check_property(cf, True);
					c.cl_statics = PMap.remove(cf.cf_name, c.cl_statics);
				} else {
					[];
				};
				b, c.cl_ordered_statics);
				c.cl_ordered_fields = List.filter(function cf: var b = keep_field(dce, cf);
				if (!(b)) {
				if (dce.debug) {
						print_endline( ^ ("[DCE] Removed field ", ^ (s_type_path(c.cl_path), ^ (".", cf.cf_name))));
					} else {
						[];
					};
					check_property(cf, False);
					c.cl_fields = PMap.remove(cf.cf_name, c.cl_fields);
				} else {
					[];
				};
				b, c.cl_ordered_fields);
				switch (c.cl_constructor) {
				case Some(cf) if (!(keep_field(dce, cf))): c.cl_constructor = None;
				case _: [];
				};
				function inef(cf) return {
					!(is_extern_field(cf));
				};
				var has_non_extern_fields = || (List.exists(inef, c.cl_ordered_fields), List.exists(inef, c.cl_ordered_statics));
				if ( || (Meta.has(Meta.Used, c.cl_meta), has_non_extern_fields)) {
					loop(::(mt, acc), l);
				} else {
					switch (c.cl_init) {
					case Some(f) if (Meta.has(Meta.KeepInit, c.cl_meta)): c.cl_extern = True;
						loop(::(mt, acc), l);
					case _: if (dce.debug) {
							print_endline( ^ ("[DCE] Removed class ", s_type_path(c.cl_path)));
						} else {
							[];
						};
						loop(acc, l);
					};
				};
			case ::(TEnumDecl(en) = mt, l) if (||(Meta.has(Meta.Used, en.e_meta), ||(en.e_extern, keep_whole_enum(dce, en)))): loop(::
						(mt, acc), l);
			case ::(TEnumDecl(e), l): if (dce.debug) {
					print_endline( ^ ("[DCE] Removed enum ", s_type_path(e.e_path)));
				} else {
					[];
				};
				loop(acc, l);
			case ::(mt, l): loop(::(mt, acc), l);
			case []: acc;
			};
		};
		com.types = loop([], List.rev(com.types));
		fix_accessors(com);
		List.iter(function mt:
		switch (mt) {
	case TClassDecl(c): c.cl_overrides = List.filter(function s: 	function loop(c) return {
				switch (c.cl_super) {
				case Some(csup, _) if (PMap.mem(s.cf_name, csup.cl_fields)): True;
				case Some(csup, _): loop(csup);
				case None: False;
				};
			};
			loop(c), c.cl_overrides);
		case _: [];
		}, com.types);
	List.iter(function case TClassDecl({ cl_extern = False; cl_super = Some({ cl_extern = True } = csup, _) }): mark_directly_used_class(csup);
				  case TClassDecl({ cl_extern = False } = c) if (<>(c.cl_implements, [])): List.iter(function (iface, _):
				if (iface.cl_extern) {
					mark_directly_used_class(iface);
					} else {
						[];
					}, c.cl_implements);
	case _: [], com.types);
		function remove_meta(m) return {
		case []: [];
		case ::((m2, _, _), l) if (=(m, m2)): l;
		case ::(x, l): ::(x, remove_meta(m, l));
		};
		List.iter(function cf: cf.cf_meta = remove_meta(Meta.Used, cf.cf_meta), dce.marked_fields);
		List.iter(function cf: cf.cf_meta = remove_meta(Meta.MaybeUsed, cf.cf_meta), dce.marked_maybe_fields);
	}
}
;
