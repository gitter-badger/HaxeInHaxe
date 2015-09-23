import Ast;
import Type;
import Common;
import Typecore;

enum Switch_mode {
	CMatch(value: Tuple<Tenum_field, Option<Option<Tuple<String, T>>>, Pos>);
	CExpr(value: Texpr);
};

enum Access_mode {
	MGet;
	MSet;
	MCall;
};

enum Identifier_type {
	ITLocal(value: Tvar);
	ITMember(value: TclassTclass_field);
	ITStatic(value: TclassTclass_field);
	ITEnum(value: TenumTenum_field);
	ITGlobal(value: Module_typeStringT);
	ITType(value: Module_type);
	ITPackage(value: String);
};

enum Display_field_kind {
	FKVar;
	FKMethod;
	FKType;
	FKPackage;
};

class /*exception*/ DisplayFields {

};

class /*exception*/ DisplayToplevel {

};

class /*exception*/ WithTypeError {

};

enum Access_kind {
	AKNo(value: String);
	AKExpr(value: Texpr);
	AKSet(value: TexprTTclass_field);
	AKInline(value: TexprTclass_fieldTfield_accessT);
	AKMacro(value: TexprTclass_field);
	AKUsing(value: TexprTclassTclass_fieldTexpr);
	AKAccess(value: TabstractTparamsTclassTexprTexpr);
};

enum Type_class {
	KInt;
	KFloat;
	KString;
	KUnk;
	KDyn;
	KOther;
	KParam(value: T);
	KAbstract(value: TabstractList<T>);
};

enum State {
	Generating;
	Done;
	NotYet;
};

enum Macro_arg_type {
	MAExpr;
	MAFunction;
	MAOther;
};

class Typer {
	public static var build_call_ref = ref(function _: function _: function _: function _: function _: assert False) : ref(
										   typer -> access_kind -> list(expr) -> with_type -> pos -> texpr);

	public static function mk_infos(ctx, p, params) return {
		var file = if (ctx.in_macro) {
			p.pfile;
		} else {
			if (Common.defined(ctx.com, Define.AbsolutePath)) {
				Common.get_full_path(p.pfile);
			} else {
				Filename.basename(p.pfile);
			};
		};
		(new Tuple(EObjectDecl(::((new Tuple("fileName", (new Tuple(EConst(String(file)), p)))), ::((new Tuple("lineNumber", (new Tuple(EConst(Int(string_of_int(Lexer.get_error_line(p)))), p)))), ::((new Tuple("className", (new Tuple(EConst(String(s_type_path(ctx.curclass.cl_path))), p)))), if ( = (ctx.curfield.cf_name, "")) {
		params;
	} else {
		::((new Tuple("methodName", (new Tuple(EConst(String(ctx.curfield.cf_name)), p)))), params);
		})))), p));
	};

	public static function check_assign(ctx, e) return {
		switch (e.eexpr) {
		case TLocal({ v_extra = None }) | TArray(_) | TField(_): [];
		case TConst(TThis) | TTypeExpr(_) if (ctx.untyped): [];
		case _: error("Invalid assign", e.epos);
		};
	};

	public static function classify(t) return {
		switch (follow(t)) {
		case TInst({ cl_path = ([], String) }, []): KString;
		case TAbstract({ a_impl = Some(_) } = a, tl): KAbstract(a, tl);
		case TAbstract({ a_path = ([], Int) }, []): KInt;
		case TAbstract({ a_path = ([], Float) }, []): KFloat;
		case TAbstract(a, []) if (List.exists(function t:
				switch (classify(t)) {
				case KInt | KFloat: True;
				case _: False;
				}, a.a_to)): KParam(t);
		case TInst({ cl_kind = KTypeParameter(ctl) }, _) if (List.exists(function t:
				switch (classify(t)) {
				case KInt | KFloat: True;
				case _: False;
				}, ctl)): KParam(t);
		case TMono(r) if (=(r.val, None)): KUnk;
		case TDynamic(_): KDyn;
		case _: KOther;
		};
	};

	public static function get_iterator_param(t) return {
		switch (follow(t)) {
		case TAnon(a): if (<>(a.a_status.val, Closed)) {
				raise(Not_found);
			} else {
				[];
			};
			switch ((new Tuple(follow(PMap.find("hasNext", a.a_fields).cf_type), follow(PMap.find("next", a.a_fields).cf_type)))) {
			case (TFun([], tb), TFun([], t)) if (switch (follow(tb)) {
					case TAbstract({ a_path = ([], Bool) }, []): True;
						case _: False;
						}):
				if (<>(PMap.fold(function _: function acc: +(acc, 1), a.a_fields, 0), 2)) {
					raise(Not_found);
				} else {
					[];
				};
				t;
			case _: raise(Not_found);
			};
		case _: raise(Not_found);
		};
	};

	public static function get_iterable_param(t) return {
		switch (follow(t)) {
		case TAnon(a): if (<>(a.a_status.val, Closed)) {
				raise(Not_found);
			} else {
				[];
			};
			switch (follow(PMap.find("iterator", a.a_fields).cf_type)) {
			case TFun([], it): var t = get_iterator_param(it);
				if (<>(PMap.fold(function _: function acc: +(acc, 1), a.a_fields, 0), 1)) {
					raise(Not_found);
				} else {
					[];
				};
				t;
			case _: raise(Not_found);
			};
		case _: raise(Not_found);
		};
	};

	public static function remove_constant_flag(t, callb) return {
		var tmp = ref([]);
		function loop(t) return {
			switch (follow(t)) {
			case TAnon(a): if ( = (a.a_status.val, Const)) {
					a.a_status.val = Closed;
					tmp.val = ::(a, tmp.val);
				} else {
					[];
				};
				PMap.iter(function _: function f: loop(f.cf_type), a.a_fields);
			case _: [];
			};
		};
		function restore([]) return {
			List.iter(function a: a.a_status.val = Const, tmp.val);
		};
		try {
			loop(t);
			var ret = callb(<>(tmp.val, []));
			restore([]);
			ret;
		} catch (e: e) {
			restore([]);
			raise(e);
		};
	};

	public static function is_pos_infos(match) return switch (match) {
	case TMono(r): switch (r.val) {
		case Some(t): is_pos_infos(t);
		case _: False;
		};
	case TLazy(f): is_pos_infos(f.val([]));
	case TType({ t_path = (::(haxe, []), PosInfos) }, []): True;
	case TType(t, tl): is_pos_infos(apply_params(t.t_params, tl, t.t_type));
	case _: False;
	};

	public static function check_constraints(ctx, tname, tpl, tl, map, delayed, p) return {
		List.iter2(function m: function (name, t):
		switch (follow(t)) {
	case TInst({ cl_kind = KTypeParameter(constr) }, _) if (<>(constr, [])): 	function f([]) return {
				List.iter(function ct:
				try {
					Type.unify(map(m), map(ct));
				} catch (e: Unify_error(l)) {
					var l = ::(Constraint_failure( ^ (tname, ^ (".", name))), l);
					raise(Unify_error(l));
				}, constr);
			};
			if (delayed) {
				delay(ctx, PCheckConstraint, function []:
				try {
					f([]);
				} catch (e: Unify_error(l)) {
					display_error(ctx, error_msg(Unify(l)), p);
				});
			} else {
				f([]);
			};
		case _: [];
		}, tl, tpl);
	};

	public static function enum_field_type(ctx, en, ef, tl_en, tl_ef, p) return {
		function map(t) return {
			apply_params(en.e_params, tl_en, apply_params(ef.ef_params, tl_ef, t));
		};
		try {
			check_constraints(ctx, s_type_path(en.e_path), en.e_params, tl_en, map, True, p);
			check_constraints(ctx, ef.ef_name, ef.ef_params, tl_ef, map, True, p);
		} catch (e: Unify_error(l)) {
			display_error(ctx, error_msg(Unify(l)), p);
		};
		map(ef.ef_type);
	};

	public static function add_constraint_checks(ctx, ctypes, pl, f, tl, p) return {
		List.iter2(function m: function (name, t):
		switch (follow(t)) {
	case TInst({ cl_kind = KTypeParameter(constr) }, _) if (<>(constr, [])): var constr = List.map(
				function t: var t = apply_params(f.cf_params, tl, t);
			var t = if ( = (pl, [])) {
			t;
		} else {
			apply_params(ctypes, pl, t);
			};
			t, constr);
			delay(ctx, PCheckConstraint, function []: List.iter(function ct:
			try {
				Type.unify(m, ct);
			} catch (e: Unify_error(l)) {
				display_error(ctx, error_msg(Unify(::(Constraint_failure( ^ (f.cf_name, ^ (".", name))), l))), p);
			}, constr));
		case _: [];
		}, tl, f.cf_params);
	};

	public static function field_type(ctx, c, pl, f, p) return {
		switch (f.cf_params) {
		case []: f.cf_type;
		case l: var monos = List.map(function _: mk_mono([]), l);
			if (!(Meta.has(Meta.Generic, f.cf_meta))) {
				add_constraint_checks(ctx, c.cl_params, pl, f, monos, p);
			} else {
				[];
			};
			apply_params(l, monos, f.cf_type);
		};
	};

	public static function class_field(ctx, c, tl, name, p) return {
		raw_class_field(function f: field_type(ctx, c, tl, f, p), c, tl, name);
	};

	public static function can_access(ctx, ? : (in_overload = False), c, cf, stat) return {
		if (cf.cf_public) {
			True;
		} else {
			if ( && (!(in_overload), && (ctx.com.config.pf_overload, Meta.has(Meta.Overload, cf.cf_meta)))) {
				True;
			} else {
				function make_path(c, f) return {
					switch (c.cl_kind) {
					case KAbstractImpl(a): @(fst(a.a_path), ::(snd(a.a_path), ::(f.cf_name, [])));
					case KGenericInstance(c, _): make_path(c, f);
					case _ if (c.cl_private): List.rev(::(f.cf_name, ::(snd(c.cl_path), List.tl(List.rev(fst(c.cl_path))))));
					case _: @(fst(c.cl_path), ::(snd(c.cl_path), ::(f.cf_name, [])));
					};
				};
				function expr_path(acc, e) return {
					switch (fst(e)) {
					case EField(e, f): expr_path(::(f, acc), e);
					case EConst(Ident(n)): ::(n, acc);
					case _: [];
					};
				};
				function chk_path(psub, pfull) return {
					switch ((new Tuple(psub, pfull))) {
					case ([], _): True;
					case (::(a, l1), ::(b, l2)) if (=(a, b)): chk_path(l1, l2);
					case _: False;
					};
				};
				function has(m, c, f, path) return {
					function loop(match) return switch (match) {
					case ::((m2, el, _), l) if (=(m, m2)): || (List.exists(function e: var p = expr_path([], e);
						&& (<>(p, []), chk_path(p, path)), el), loop(l));
					case ::(_, l): loop(l);
					case []: False;
					};
					|| (loop(c.cl_meta), loop(f.cf_meta));
				};
				var cur_paths = ref([]);
				function loop(c) return {
					cur_paths.val = ::(make_path(c, ctx.curfield), cur_paths.val);
					switch (c.cl_super) {
					case Some(csup, _): loop(csup);
					case None: [];
					};
					List.iter(function (c, _): loop(c), c.cl_implements);
				};
				loop(ctx.curclass);
				var is_constr = = (cf.cf_name, "new");
				function loop(c) return {
					|| (try {
						var f = if (is_constr) {
							switch (c.cl_constructor) {
							case None: raise(Not_found);
							case Some(c): c;
							};
						} else {
							PMap.find(cf.cf_name, if (stat) {
							c.cl_statics;
						} else {
							c.cl_fields;
						});
						};
						|| (is_parent(c, ctx.curclass), List.exists(has(Meta.Allow, c, f), cur_paths.val));
					} catch (e: Not_found) {
						False;
					}, || (switch (c.cl_super) {
				case Some(csup, _): loop(csup);
					case None: False;
					}, has(Meta.Access, ctx.curclass, ctx.curfield, make_path(c, cf))));
				};
				var b = || (loop(c), || (switch (c.cl_kind) {
			case KTypeParameter(tl): List.exists(function t: switch (follow(t)) {
				case TInst(c, _): loop(c);
					case _: False;
					}, tl);
				case _: False;
				}, Meta.has(Meta.PrivateAccess, ctx.meta)));
				if ( && (b, && (Common.defined(ctx.com, Common.Define.As3), !(Meta.has(Meta.Public, cf.cf_meta))))) {
					cf.cf_meta = ::((new Tuple(Meta.Public, [], cf.cf_pos)), cf.cf_meta);
				} else {
					[];
				};
				b;
			};
		};
	};

	public static function prepare_using_field(cf) return {
		switch (cf.cf_type) {
		case TFun(::((_, _, tf), args), ret): 	function loop(acc, overloads) return {
				switch (overloads) {
				case ::( {
						cf_type = TFun(::((_, _, tfo), args), ret)
					} = cfo, l): var tfo = apply_params(cfo.cf_params, List.map(snd, cfo.cf_params), tfo);
					if (Type.type_iseq(tf, tfo)) {
						loop(::({ (cfo) with cf_type = TFun(args, ret) }, acc), l);
					} else {
						loop(acc, l);
					};
				case ::(_, l): loop(acc, l);
				case []: acc;
				};
			};
			{
				(cf) with cf_overloads = loop([], cf.cf_overloads);
				cf_type = TFun(args, ret)
			};
		case _: cf;
		};
	};

	public static function parse_string(com, s, p, inlined) return {
		var old = Lexer.save([]);
		var old_file = try {
			Some(Hashtbl.find(Lexer.all_files, p.pfile));
		} catch (e: Not_found) {
			None;
		};
		var old_display = Parser.resume_display.val;
		var old_de = Parser.display_error.val;
		function restore([]) return {
			switch (old_file) {
			case None: [];
			case Some(f): Hashtbl.replace(Lexer.all_files, p.pfile, f);
			};
			if (!(inlined)) {
				Parser.resume_display.val = old_display;
			} else {
				[];
			};
			Lexer.restore(old);
			Parser.display_error.val = old_de;
		};
		Lexer.init(p.pfile, True);
		Parser.display_error.val = function e: function p: raise(Parser.Error(e, p));
		if (!(inlined)) {
			Parser.resume_display.val = null_pos;
		} else {
			[];
		};
		var Tuple(pack, decls) = try {
			Parser.parse(com, Lexing.from_string(s));
		} catch (e: T) {
			McOr(McArr(PaApp(PaApp(PaId(<...>), PaId(<...>)), PaId(IdLid(<...>))), ExNil, ExSeq(ExSem(ExApp(<...>, <...>), ExApp(<...>,
			<...>)))), McArr(PaApp(PaApp(PaId(<...>), PaId(<...>)), PaId(IdLid(<...>))), ExNil, ExSeq(ExSem(ExApp(<...>, <...>),
		ExApp(<...>, <...>)))))			case Parser.Error(e, pe): restore([]);
			error(Parser.error_msg(e), if (inlined) {
			pe;
		} else {
			p;
		});
		case Lexer.Error(e, pe): restore([]);
			error(Lexer.error_msg(e), if (inlined) {
			pe;
		} else {
			p;
		});
		};
		restore([]);
		(new Tuple(pack, decls));
	};

	public static function eval(ctx, s) return {
		var p = {
			() with pfile = "--eval";
			pmin = 0;
			pmax = String.length(s)
		};
		var Tuple(pack, decls) = parse_string(ctx.com, s, p, False);
		function find_main(current, decls) return {
			switch (decls) {
			case ::((EClass(c), _), decls): var path = (new Tuple(pack, c.d_name));
				try {
					var cff = List.find(function cff: = (cff.cff_name, "main"), c.d_data);
					if (<>(ctx.com.main_class, None)) {
						error("Multiple main", cff.cff_pos);
					} else {
						[];
					};
					ctx.com.main_class = Some(path);
					Some(path);
				} catch (e: Not_found) {
					find_main(if ( = (current, None)) {
					Some(path);
					} else {
						current;
					}, decls);
				};
			case ::((EEnum({ d_name = s }) | ETypedef({ d_name = s }) | EAbstract({ d_name = s }), _), decls) if (=(current, None)):
				find_main(Some(pack, s), decls);
			case ::(_, decls): find_main(current, decls);
			case []: current;
			};
		};
		var path_module = switch (find_main(None, decls)) {
		case None: error("Evaluated string did not define any types", p);
		case Some(path): path;
		};
		ignore(Typeload.type_module(ctx, path_module, "eval", decls, p));
		flush_pass(ctx, PBuildClass, "eval");
	};

	public static function parse_expr_string(ctx, s, p, inl) return {
		var head = "class X{static function main[] ";
		var head = if ( > (p.pmin, String.length(head))) {
			^ (head, String.make(-(p.pmin, String.length(head)), ' '));
		} else {
			head;
		};
		function loop(e) return {
			var e = Ast.map_expr(loop, e);
			(new Tuple(fst(e), p));
		};
		switch (parse_string(ctx.com, ^ (head, ^ (s, ";}")), p, inl)) {
		case (_, ::((EClass({ d_data = ::({ cff_name = main; cff_kind = FFun({ f_expr = Some(e) }) }, []) }), _), [])):
			if (inl) {
				e;
			} else {
				loop(e);
			};
		case _: raise(Interp.Invalid_expr);
		};
	};

	public static function collect_toplevel_identifiers(ctx) return {
		var acc = DynArray.create([]);
		PMap.iter(function _: function v:
		if (!(is_gen_local(v))) {
		DynArray.add(acc, ITLocal(v));
		} else {
			[];
		}, ctx.locals);
		if (<>(ctx.curfun, FunStatic)) {
			function loop(c) return {
				List.iter(function cf: DynArray.add(acc, ITMember(ctx.curclass, cf)), c.cl_ordered_fields);
				switch (c.cl_super) {
				case None: [];
				case Some(csup, tl): loop(csup);
				};
			};
			loop(ctx.curclass);
		} else {
			[];
		};
		List.iter(function cf: DynArray.add(acc, ITStatic(ctx.curclass, cf)), ctx.curclass.cl_ordered_statics);
		function enum_ctors(t) return {
			switch (t) {
			case TClassDecl(_) | TAbstractDecl(_): [];
			case TTypeDecl(t): switch (follow(t.t_type)) {
				case TEnum(e, _): enum_ctors(TEnumDecl(e));
				case _: [];
				};
			case TEnumDecl(e): PMap.iter(function _: function ef: DynArray.add(acc, ITEnum(e, ef)), e.e_constrs);
			};
		};
		List.iter(enum_ctors, ctx.m.curmod.m_types);
		List.iter(enum_ctors, ctx.m.module_types);
		PMap.iter(function _: function (mt, s):
		try {
			var t = switch (Typeload.resolve_typedef(mt)) {
			case TClassDecl(c): PMap.find(s, c.cl_statics).cf_type;
			case TEnumDecl(en): PMap.find(s, en.e_constrs).ef_type;
			case TAbstractDecl({ a_impl = Some(c) }): PMap.find(s, c.cl_statics).cf_type;
			case _: raise(Not_found);
			};
			DynArray.add(acc, ITGlobal(mt, s, t));
		} catch (e: Not_found) {
			[];
		}, ctx.m.module_globals);
		var module_types = ref([]);
		function add_type(mt) return {
			switch (mt) {
			case TClassDecl({ cl_kind = KAbstractImpl(_) }): [];
			case _: var path = t_infos(mt).mt_path;
				if (!(List.exists(function mt2: = (t_infos(mt2).mt_path, path), module_types.val))) {
					module_types.val = ::(mt, module_types.val);
				} else {
					[];
				};
			};
		};
		List.iter(add_type, ctx.m.curmod.m_types);
		List.iter(add_type, ctx.m.module_types);
		List.iter(function c: add_type(TClassDecl(c)), ctx.m.module_using);
		var class_paths = ctx.com.class_path;
		var class_paths = List.filter(function s: <>(s, ""), class_paths);
		var packages = ref([]);
		function add_package(pack) return {
			try {
				switch (PMap.find(pack, ctx.com.package_rules)) {
				case Forbidden: [];
				case _: raise(Not_found);
				};
			} catch (e: Not_found) {
				if (!(List.mem(pack, packages.val))) {
					packages.val = ::(pack, packages.val);
				} else {
					[];
				};
			};
		};
		List.iter(function dir:
		try {
			var entries = Sys.readdir(dir);
			Array.iter(function file:
			switch (file) {
		case . | ..: [];
			case _ if (Sys.is_directory(^(dir, file))): add_package(file);
			case _: var l = String.length(file);
				if ( && ( > (l, 3), = (String.sub(file, -(l, 3), 3), ".hx"))) {
					try {
						var name = String.sub(file, 0, -(l, 3));
						var md = Typeload.load_module(ctx, (new Tuple([], name)), Ast.null_pos);
						List.iter(function mt:
						if ( = (t_infos(mt).mt_path, md.m_path)) {
						add_type(mt);
						} else {
							[];
						}, md.m_types);
					} catch (e: _) {
						[];
					};
				} else {
					[];
				};
			}, entries);
		} catch (e: Sys_error(_)) {
			[];
		}, class_paths);
		List.iter(function pack: DynArray.add(acc, ITPackage(pack)), packages.val);
		List.iter(function mt: DynArray.add(acc, ITType(mt)), module_types.val);
		raise(DisplayToplevel(DynArray.to_list(acc)));
	};

	public static function unify_min_raise(ctx, ellist(texpr)) return {
		function base_types(t) return {
			var tl = ref([]);
			function loop(t) return {
				switch (t) {
				case TInst(cl, params): switch (cl.cl_kind) {
					case KTypeParameter(tl): List.iter(loop, tl);
					case _: [];
					};
					List.iter(function (ic, ip): var t = apply_params(cl.cl_params, params, TInst(ic, ip));
							  loop(t), cl.cl_implements);
					switch (cl.cl_super) {
					case None: [];
					case Some(csup, pl): var t = apply_params(cl.cl_params, params, TInst(csup, pl));
						loop(t);
					};
					tl.val = ::(t, tl.val);
				case TEnum(en, ::(_, _) = tl2): tl.val = ::(TEnum(en, List.map(function _: t_dynamic, tl2)), tl.val);
					tl.val = ::(t, tl.val);
				case TType(td, pl): loop(apply_params(td.t_params, pl, td.t_type));
					tl.val = ::(t, tl.val);
				case TLazy(f): loop(f.val([]));
				case TMono(r): switch (r.val) {
					case None: [];
					case Some(t): loop(t);
					};
				case _: tl.val = ::(t, tl.val);
				};
			};
			loop(t);
			tl.val;
		};
		switch (el) {
		case []: mk_mono([]);
		case ::(e, []): e.etype;
		case _: 	function chk_null(e) return {
				|| (is_null(e.etype), switch (e.eexpr) {
			case TConst(TNull): True;
				case TBlock(el): switch (List.rev(el)) {
					case []: False;
					case ::(e, _): chk_null(e);
					};
				case TParenthesis(e) | TMeta(_, e): chk_null(e);
				case _: False;
				});
			};
			function loop(t) return {
			case []: (new Tuple(False, t));
			case ::(e, el): var t = if (chk_null(e)) {
					ctx.t.tnull(t);
				} else {
					t;
				};
				try {
					unify_raise(ctx, e.etype, t, e.epos);
					loop(t, el);
				} catch (e: Error(Unify(_))(_)) {
					try {
						unify_raise(ctx, t, e.etype, e.epos);
						loop(if (is_null(t)) {
						ctx.t.tnull(e.etype);
						} else {
							e.etype;
						}, el);
					} catch (e: Error(Unify(_))(_)) {
						(new Tuple(True, t));
					};
				};
			};
			var Tuple(has_error, t) = loop(mk_mono([]), el);
			if (!(has_error)) {
				t;
			} else {
				try {
					var fcount = ref(-1);
					function field_count(a) return {
						PMap.fold(function _: function acc: +(acc, 1), a.a_fields, 0);
					};
					function expr(f) return {
						switch (f.cf_expr) {
						case None: mk(TBlock([]), f.cf_type, f.cf_pos);
						case Some(e): e;
						};
					};
					var fields = List.fold_left(function acc: function e:
					switch (follow(e.etype)) {
				case TAnon(a) if (=(a.a_status.val, Const)):
						if ( = (fcount.val, -1)) {
							fcount.val = field_count(a);
							PMap.map(function f: ::(expr(f), []), a.a_fields);
						} else {
							if (<>(fcount.val, field_count(a))) {
								raise(Not_found);
							} else {
								[];
							};
							PMap.mapi(function n: function el: ::(expr(PMap.find(n, a.a_fields)), el), acc);
						};
					case _: raise(Not_found);
					}, PMap.empty, el);
					var fields = PMap.foldi(function n: function el: function acc: var t = try {
						unify_min_raise(ctx, el);
					} catch (e: Error(Unify(_))(_)) {
						raise(Not_found);
					};
					PMap.add(n, mk_field(n, t, List.hd(el).epos), acc), fields, PMap.empty);
					TAnon({ () with a_fields = fields;
							a_status = ref(Closed)
						  });
				} catch (e: Not_found) {
					var common_types = base_types(t);
					var dyn_types = List.fold_left(function acc: function t: 	function loop(c) return {
						|| (Meta.has(Meta.UnifyMinDynamic, c.cl_meta), switch (c.cl_super) {
					case None: False;
					case Some(c, _): loop(c);
						});
					};
					switch (t) {
				case TInst(c, params) if (&&(<>(params, []), loop(c))): ::(TInst(c, List.map(function _: t_dynamic, params)), acc);
					case _: acc;
					}, [], common_types);
					var common_types = ref(switch (List.rev(dyn_types)) {
				case []: common_types;
					case l: @(common_types, l);
					});
					function loop(e) return {
						var first_error = ref(None);
						function filter(t) return {
							try {
								unify_raise(ctx, e.etype, t, e.epos);
								True;
							} catch (e: Error(Unify(l))(p) = err) {
								if ( = (first_error.val, None)) {
									first_error.val = Some(err);
								} else {
									[];
								};
								False;
							};
						};
						common_types.val = List.filter(filter, common_types.val);
						switch ((new Tuple(common_types.val, first_error.val))) {
						case ([], Some(err)): raise(err);
						case _: [];
						};
					};
					switch (common_types.val) {
					case []: error("No common base type found", punion(List.hd(el).epos, List.hd(List.rev(el)).epos));
					case _: List.iter(loop, List.tl(el));
						List.hd(common_types.val);
					};
				};
			};
		} : t;
	};

	public static function unify_min(ctx, el) return {
		try {
			unify_min_raise(ctx, el);
		} catch (e: Error(Unify(l))(p)) {
			if (!(ctx.untyped)) {
				display_error(ctx, error_msg(Unify(l)), p);
			} else {
				[];
			};
			List.hd(el).etype;
		};
	};

	public static function is_forced_inline(c, cf) return {
		switch (c) {
		case Some({ cl_extern = True }): True;
		case Some({ cl_kind = KAbstractImpl(_) }): True;
		case _ if (Meta.has(Meta.Extern, cf.cf_meta)): True;
		case _: False;
		};
	};

	public static function unify_call_args'(ctx, el, args, r, callp, inline, force_inline) return {
	function call_error(err, p) return {
	raise(Error(Call_error(err), p));
};
	function arg_error(ul, name, opt, p) return {
	var err = Stack(Unify(ul), Custom(^("For ", ^(if (opt) {
	"optional ";
} else {
	"";
}, ^("function argument '", ^(name, "'"))))));
	call_error(Could_not_unify(err), p);
};
	function mk_pos_infos(t) return {
	var infos = mk_infos(ctx, callp, []);
	type_expr(ctx, infos, WithType(t));
};
	function default_value(name, t) return {
	if (is_pos_infos(t)) {
	mk_pos_infos(t);
} else {
	null(ctx.t.tnull(t), callp);
};
};
	var skipped = ref([]);
	function skip(name, ul, t) return {
	if (&&(!(ctx.com.config.pf_can_skip_non_nullable_argument), !(is_nullable(t)))) {
	call_error(Cannot_skip_non_nullable(name), callp);
} else {
	[];
};
	skipped.val = ::((new Tuple(name, ul)), skipped.val);
	default_value(name, t);
};
	function type_against(t, e) return {
	var e = type_expr(ctx, e, WithTypeResume(t));
	try {
	Codegen.AbstractCast.cast_or_unify_raise(ctx, t, e, e.epos);
} catch(e:Error(Unify(l))(p)) {
	raise(WithTypeError(l, p));
};
};
	function loop(el, args) return {
	switch ((new Tuple(el, args))) {
	case ([], []): [];
	case (_, ::((name, False, t), [])) if(switch (follow(t)) {
	case TAbstract({ a_path = (::(haxe, ::(extern, [])), Rest) }, _): True;
	case _: False;
}): switch (follow(t)) {
	case TAbstract({ a_path = (::(haxe, ::(extern, [])), Rest) }, ::(t, [])): try {
	List.map(function e: (new Tuple(type_against(t, e), False)), el);
} catch(e:WithTypeError(ul)(p)) {
	arg_error(ul, name, False, p);
};
	case _: assert False;
};
	case ([], ::((_, False, _), _)): call_error(Not_enough_arguments(args), callp);
	case ([], ::((name, True, t), args)): switch (loop([], args)) {
	case [] if(&&(!(&&(inline, ||(ctx.g.doinline, force_inline))), !(ctx.com.config.pf_pad_nulls))): if (is_pos_infos(t)) {
	::((new Tuple(mk_pos_infos(t), True)), []);
} else {
	[];
};
	case args: var e_def = default_value(name, t);
	::((new Tuple(e_def, True)), args);
};
	case (::((_, p), _), []): switch (List.rev(skipped.val)) {
	case []: call_error(Too_many_arguments, p);
	case ::((s, ul), _): arg_error(ul, s, True, p);
};
	case (::(e, el), ::((name, opt, t), args)): try {
	var e = type_against(t, e);
	::((new Tuple(e, opt)), loop(el, args));
} catch(e:WithTypeError(ul)(p)) {
	if (opt) {
	var e_def = skip(name, ul, t);
	::((new Tuple(e_def, True)), loop(::(e, el), args));
} else {
	arg_error(ul, name, False, p);
};
};
};
};
	var el = loop(el, args);
	(new Tuple(el, TFun(args, r)));
};

	public static function unify_call_args(ctx, el, args, r, p, inline, force_inline) return {
	var Tuple(el, tf) = unify_call_args'(ctx, el, args, r, p, inline, force_inline);
	(new Tuple(List.map(fst, el), tf));
};

public static function unify_field_call(ctx, fa, el, args, ret, p, inline) return {
	function map_cf(cf0, map, cf) return {
		var t = map(monomorphs(cf.cf_params, cf.cf_type));
		switch ((new Tuple(cf.cf_expr, cf.cf_kind))) {
		case (None, Method(MethInline)) if (!(ctx.com.config.pf_overload)): cf.cf_expr = cf0.cf_expr;
			cf.cf_kind = cf0.cf_kind;
		case _: [];
		};
		(new Tuple(t, cf));
	};
	function expand_overloads(map, cf) return {
		::((new Tuple(TFun(args, ret), cf)), List.map(map_cf(cf, map), cf.cf_overloads));
	};
	var Tuple(candidates, co, cf, mk_fa) = switch (fa) {
	case FStatic(c, cf): (new Tuple(expand_overloads(function t: t, cf), Some(c), cf, function cf: FStatic(c, cf)));
	case FAnon(cf): (new Tuple(expand_overloads(function t: t, cf), None, cf, function cf: FAnon(cf)));
	case FInstance(c, tl, cf): var map = apply_params(c.cl_params, tl);
		var cfl = if ( || ( = (cf.cf_name, "new"), !( && (Meta.has(Meta.Overload, cf.cf_meta), ctx.com.config.pf_overload)))) {
			List.map(map_cf(cf, map), cf.cf_overloads);
		} else {
			List.map(function (t, cf): (new Tuple(map(monomorphs(cf.cf_params, t)), cf)), Typeload.get_overloads(c, cf.cf_name));
		};
		(new Tuple(::((new Tuple(TFun(args, ret), cf)), cfl), Some(c), cf, function cf: FInstance(c, tl, cf)));
	case FClosure(co, cf): var c = switch (co) {
		case None: None;
		case Some(c, _): Some(c);
		};
		(new Tuple(expand_overloads(function t: t, cf), c, cf, function cf:
		switch (co) {
	case None: FAnon(cf);
		case Some(c, tl): FInstance(c, tl, cf);
		}));
	case _: error("Invalid field call", p);
	};
	var is_forced_inline = is_forced_inline(co, cf);
	var is_overload = Meta.has(Meta.Overload, cf.cf_meta);
	function attempt_call(t, cf) return {
		switch (follow(t)) {
		case TFun(args, ret): var Tuple(el, tf) = unify_call_args'(ctx, el, args, ret, p, inline, is_forced_inline);
			function mk_call(ethis, p_field) return {
			var ef = mk(TField(ethis, mk_fa(cf)), tf, p_field);
			make_call(ctx, ef, List.map(fst, el), ret, p);
		};
			(new Tuple(el, tf, mk_call));
			case _: assert False;
		};
		};
			function loop(candidates) return {
			switch (candidates) {
			case []: (new Tuple([], []));
			case ::((t, cf), candidates): try {
			var candidate = attempt_call(t, cf);
			if (&&(ctx.com.config.pf_overload, is_overload)) {
			var Tuple(candidates, failures) = loop(candidates);
			(new Tuple(::(candidate, candidates), failures));
		} else {
			(new Tuple(::(candidate, []), []));
		};
		} catch(e:Error(Call_error(_))(_) = err) {
			var Tuple(candidates, failures) = loop(candidates);
			(new Tuple(candidates, ::(err, failures)));
		};
		};
		};
			switch (candidates) {
			case ::((t, cf), []): var Tuple(el, tf, mk_call) = attempt_call(t, cf);
			(new Tuple(List.map(fst, el), tf, mk_call));
			case _: var Tuple(candidates, failures) = loop(candidates);
			function fail([]) return {
			switch (List.rev(failures)) {
			case ::(err, _): raise(err);
			case _: assert False;
		};
		};
			if (&&(is_overload, ctx.com.config.pf_overload)) {
			switch (Codegen.Overloads.reduce_compatible(candidates)) {
			case []: fail([]);
			case ::((el, tf, mk_call), []): (new Tuple(List.map(fst, el), tf, mk_call));
			case _: error("Ambiguous overload", p);
		};
		} else {
			switch (List.rev(candidates)) {
			case []: fail([]);
			case ::((el, tf, mk_call), _): (new Tuple(List.map(fst, el), tf, mk_call));
		};
		};
		};
		};

			public static function fast_enum_field(e, ef, p) return {
			var et = mk(TTypeExpr(TEnumDecl(e)), TAnon({ () with a_fields = PMap.empty;
			a_status = ref(EnumStatics(e)) }), p);
			TField(et, FEnum(e, ef));
		};

			public static function type_module_type(ctx, t, tparams, p) return {
			switch (t) {
			case TClassDecl(c): var t_tmp = { () with t_path = (new Tuple([], ^("Class<", ^(s_type_path(c.cl_path), ">"))));
			t_module = c.cl_module;
			t_doc = None;
			t_pos = c.cl_pos;
			t_type = TAnon({ () with a_fields = c.cl_statics;
			a_status = ref(Statics(c)) });
			t_private = True;
			t_params = [];
			t_meta = no_meta };
			mk(TTypeExpr(TClassDecl(c)), TType(t_tmp, []), p);
			case TEnumDecl(e): var types = switch (tparams) {
			case None: List.map(function _: mk_mono([]), e.e_params);
			case Some(l): l;
		};
			mk(TTypeExpr(TEnumDecl(e)), TType(e.e_type, types), p);
			case TTypeDecl(s): var t = apply_params(s.t_params, List.map(function _: mk_mono([]), s.t_params), s.t_type);
			if (!(Common.defined(ctx.com, Define.NoDeprecationWarnings))) {
			Codegen.DeprecationCheck.check_typedef(ctx.com, s, p);
		} else {
			[];
		};
			switch (follow(t)) {
			case TEnum(e, params): type_module_type(ctx, TEnumDecl(e), Some(params), p);
			case TInst(c, params): type_module_type(ctx, TClassDecl(c), Some(params), p);
			case TAbstract(a, params): type_module_type(ctx, TAbstractDecl(a), Some(params), p);
			case _: error(^(s_type_path(s.t_path), " is not a value"), p);
		};
			case TAbstractDecl({ a_impl = Some(c) }): type_module_type(ctx, TClassDecl(c), tparams, p);
			case TAbstractDecl(a): if (!(Meta.has(Meta.RuntimeValue, a.a_meta))) {
			error(^(s_type_path(a.a_path), " is not a value"), p);
		} else {
			[];
		};
			var t_tmp = { () with t_path = (new Tuple([], ^("Abstract<", ^(s_type_path(a.a_path), ">"))));
			t_module = a.a_module;
			t_doc = None;
			t_pos = a.a_pos;
			t_type = TAnon({ () with a_fields = PMap.empty;
			a_status = ref(AbstractStatics(a)) });
			t_private = True;
			t_params = [];
			t_meta = no_meta };
			mk(TTypeExpr(TAbstractDecl(a)), TType(t_tmp, []), p);
		};
		};

			public static function type_type(ctx, tpath, p) return {
			type_module_type(ctx, Typeload.load_type_def(ctx, p, { () with tpackage = fst(tpath);
			tname = snd(tpath);
			tparams = [];
			tsub = None }), None, p);
		};

			public static function get_constructor(ctx, c, params, p) return {
			switch (c.cl_kind) {
			case KAbstractImpl(a): var f = try {
			PMap.find("_new", c.cl_statics);
		} catch(e:Not_found) {
			error(^(s_type_path(a.a_path), " does not have a constructor"), p);
		};
			var ct = field_type(ctx, c, params, f, p);
			(new Tuple(apply_params(a.a_params, params, ct), f));
			case _: var Tuple(ct, f) = try {
			Type.get_constructor(function f: field_type(ctx, c, params, f, p), c);
		} catch(e:Not_found) {
			error(^(s_type_path(c.cl_path), " does not have a constructor"), p);
		};
			(new Tuple(apply_params(c.cl_params, params, ct), f));
		};
		};

			public static function make_call(ctx, e, params, t, p) return {
			try {
			var Tuple(ethis, cl, f) = switch (e.eexpr) {
			case TField(ethis, fa): var Tuple(co, cf) = switch (fa) {
			case FInstance(c, _, cf) | FStatic(c, cf): (new Tuple(Some(c), cf));
			case FAnon(cf): (new Tuple(None, cf));
			case _: raise(Exit);
		};
			(new Tuple(ethis, co, cf));
			case _: raise(Exit);
		};
			if (<>(f.cf_kind, Method(MethInline))) {
			raise(Exit);
		} else {
			[];
		};
			var config = switch (cl) {
			case Some({ cl_kind = KAbstractImpl(_) }) if(Meta.has(Meta.Impl, f.cf_meta)): var t = if (=(f.cf_name, "_new")) {
			t;
		} else {
			if (=(params, [])) {
			error("Invalid abstract implementation function", f.cf_pos);
		} else {
			follow(List.hd(params).etype);
		};
		};
			switch (t) {
			case TAbstract(a, pl): var has_params = ||(<>(a.a_params, []), <>(f.cf_params, []));
			var monos = List.map(function _: mk_mono([]), f.cf_params);
			function map_type(t) return {
			apply_params(a.a_params, pl, apply_params(f.cf_params, monos, t));
		};
			Some(has_params, map_type);
			case _: None;
		};
			case _: None;
		};
			ignore(follow(f.cf_type));
			var params = List.map(ctx.g.do_optimize(ctx), params);
			var force_inline = is_forced_inline(cl, f);
			switch (f.cf_expr) {
			case Some({ eexpr = TFunction(fd) }): switch (Optimizer.type_inline(ctx, f, fd, ethis, params, t, config, p, force_inline)) {
			case None: if (force_inline) {
			error("Inline could not be done", p);
		} else {
			[];
		};
			raise(Exit);
			case Some(e): e;
		};
			case _: raise(Exit);
		};
		} catch(e:Exit) {
			mk(TCall(e, params), t, p);
		};
		};

			public static function mk_array_get_call(ctx, Tuple(cf, tf, r, e1, e2o), c, ebase, p) return {
			switch (cf.cf_expr) {
			case None: if (&&(!(Meta.has(Meta.NoExpr, cf.cf_meta)), =(ctx.com.display, DMNone))) {
			display_error(ctx, "Recursive array get method", p);
		} else {
			[];
		};
			mk(TArray(ebase, e1), r, p);
			case Some(_): var et = type_module_type(ctx, TClassDecl(c), None, p);
			var ef = mk(TField(et, FStatic(c, cf)), tf, p);
			make_call(ctx, ef, ::(ebase, ::(e1, [])), r, p);
		};
		};

			public static function mk_array_set_call(ctx, Tuple(cf, tf, r, e1, e2o), c, ebase, p) return {
			var evalue = switch (e2o) {
			case None: assert False;
			case Some(e): e;
		};
			switch (cf.cf_expr) {
			case None: if (&&(!(Meta.has(Meta.NoExpr, cf.cf_meta)), =(ctx.com.display, DMNone))) {
			display_error(ctx, "Recursive array set method", p);
		} else {
			[];
		};
			var ea = mk(TArray(ebase, e1), r, p);
			mk(TBinop(OpAssign, ea, evalue), r, p);
			case Some(_): var et = type_module_type(ctx, TClassDecl(c), None, p);
			var ef = mk(TField(et, FStatic(c, cf)), tf, p);
			make_call(ctx, ef, ::(ebase, ::(e1, ::(evalue, []))), r, p);
		};
		};

			public static function acc_get(ctx, g, p) return {
			switch (g) {
			case AKNo(f): error(^("Field ", ^(f, " cannot be accessed for reading")), p);
			case AKExpr(e): e;
			case AKSet(_) | AKAccess(_): assert False;
			case AKUsing(et, c, cf, e) if(ctx.in_display): var ec = type_module_type(ctx, TClassDecl(c), None, p);
			var t = switch (follow(et.etype)) {
			case TFun(::(_, args), ret): TFun(args, ret);
			case _: et.etype;
		};
			mk(TField(ec, FStatic(c, cf)), t, et.epos);
			case AKUsing(et, _, cf, e): switch (follow(et.etype)) {
			case TFun(::(_, args), ret): var tcallb = TFun(args, ret);
			var twrap = TFun(::((new Tuple("_e", False, e.etype)), []), tcallb);
			var args = List.map(function (n, o, t): var t = if (o) {
			ctx.t.tnull(t);
		} else {
			t;
		};
			(new Tuple(o, if (=(n, "")) {
			gen_local(ctx, t);
		} else {
			alloc_var(n, t);
		})), args);
			var ve = alloc_var("_e", e.etype);
			var ecall = make_call(ctx, et, List.map(function v: mk(TLocal(v), v.v_type, p), ::(ve, List.map(snd, args))), ret, p);
			var ecallb = mk(TFunction({ () with tf_args = List.map(function (o, v): (new Tuple(v, if (o) {
			Some(TNull);
		} else {
			None;
		})), args);
			tf_type = ret;
			tf_expr = switch (follow(ret)) {
			case TAbstract({ a_path = ([], Void) }, _): ecall;
			case _: mk(TReturn(Some(ecall)), t_dynamic, p);
		} }), tcallb, p);
			var ewrap = mk(TFunction({ () with tf_args = ::((new Tuple(ve, None)), []);
			tf_type = tcallb;
			tf_expr = mk(TReturn(Some(ecallb)), t_dynamic, p) }), twrap, p);
			make_call(ctx, ewrap, ::(e, []), tcallb, p);
			case _: assert False;
		};
			case AKInline(e, f, fmode, t): var cmode = switch (fmode) {
			case FStatic(_): fmode;
			case FInstance(c, tl, f): FClosure(Some(c, tl), f);
			case _: assert False;
		};
			ignore(follow(f.cf_type));
			switch (f.cf_expr) {
			case None: if (<>(ctx.com.display, DMNone)) {
			mk(TField(e, cmode), t, p);
		} else {
			error("Recursive inline is not supported", p);
		};
			case Some({ eexpr = TFunction(_) }): 	function chk_class(c) return {
			&&(||(c.cl_extern, Meta.has(Meta.Extern, f.cf_meta)), !(Meta.has(Meta.Runtime, f.cf_meta)));
		};
			function wrap_extern(c) return {
			var c2 = var m = c.cl_module;
			var mpath = (new Tuple(@(fst(m.m_path), ::(^("_", snd(m.m_path)), [])), ^(snd(m.m_path), "_Impl_")));
			try {
			function loop(mtl) return {
			switch (mtl) {
			case ::(TClassDecl(c), _) if(=(c.cl_path, mpath)): c;
			case ::(_, mtl): loop(mtl);
			case []: raise(Not_found);
		};
		};
			loop(c.cl_module.m_types);
		} catch(e:Not_found) {
			var c2 = mk_class(c.cl_module, mpath, c.cl_pos);
			c.cl_module.m_types = ::(TClassDecl(c2), c.cl_module.m_types);
			c2;
		};
			var cf = try {
			PMap.find(f.cf_name, c2.cl_statics);
		} catch(e:Not_found) {
			var cf = { (f) with cf_kind = Method(MethNormal) };
			c2.cl_statics = PMap.add(cf.cf_name, cf, c2.cl_statics);
			c2.cl_ordered_statics = ::(cf, c2.cl_ordered_statics);
			cf;
		};
			var e_t = type_module_type(ctx, TClassDecl(c2), None, p);
			mk(TField(e_t, FStatic(c2, cf)), t, p);
		};
			var e_def = mk(TField(e, cmode), t, p);
			switch (follow(e.etype)) {
			case TInst(c, _) if(chk_class(c)): display_error(ctx, "Can't create closure on an extern inline member method", p);
			e_def;
			case TAnon(a): switch (a.a_status.val) {
			case Statics({ cl_extern = False }) if(Meta.has(Meta.Extern, f.cf_meta)): display_error(ctx, "Cannot create closure on @:
			extern inline method", p);
			e_def;
			case Statics(c) if(chk_class(c)): wrap_extern(c);
			case _: e_def;
		};
			case _: e_def;
		};
			case Some(e): 	function loop(e) return {
			Type.map_expr(loop, { (e) with epos = p });
		};
			loop(e);
		};
			case AKMacro(_): assert False;
		};
		};

			public static function error_require(r, p) return {
			if (=(r, "")) {
			error("This field is not available with the current compilation flags", p);
		} else {
			var r = if (=(r, "sys")) {
			"a system platform [php, neko, cpp, etc.]";
		} else {
			try {
			if (<>(String.sub(r, 0, 5), "flash")) {
			raise(Exit);
		} else {
			[];
		};
			var Tuple(_, v) = ExtString.String.replace(String.sub(r, 5, -(String.length(r), 5)), "_", ".");
			^("flash version ", ^(v, ^(", [use - swf - version ", ^(v, "]"))));
		} catch(e:_) {
			^("'", ^(r, "' to be enabled"));
		};
		};
			error(^("Accessing this field requires ", r), p);
		};
		};

			public static function get_this(ctx, p) return {
			switch (ctx.curfun) {
			case FunStatic: error("Cannot access this from a static function", p);
			case FunMemberClassLocal | FunMemberAbstractLocal: var v = switch (ctx.vthis) {
			case None: var v = if (=(ctx.curfun, FunMemberAbstractLocal)) {
			PMap.find("this", ctx.locals);
		} else {
			gen_local(ctx, ctx.tthis);
		};
			ctx.vthis = Some(v);
			v;
			case Some(v): ctx.locals = PMap.add(v.v_name, v, ctx.locals);
			v;
		};
			mk(TLocal(v), ctx.tthis, p);
			case FunMemberAbstract: var v = try {
			PMap.find("this", ctx.locals);
		} catch(e:Not_found) {
			assert False;
		};
			mk(TLocal(v), v.v_type, p);
			case FunConstructor | FunMember: mk(TConst(TThis), ctx.tthis, p);
		};
		};

			public static function field_access(ctx, mode, f, fmode, t, e, p) return {
			function fnormal([]) return {
			AKExpr(mk(TField(e, fmode), t, p));
		};
			function normal([]) return {
			switch (follow(e.etype)) {
			case TAnon(a): switch (a.a_status.val) {
			case EnumStatics(en): var c = try {
			PMap.find(f.cf_name, en.e_constrs);
		} catch(e:Not_found) {
			assert False;
		};
			var fmode = FEnum(en, c);
			AKExpr(mk(TField(e, fmode), t, p));
			case _: fnormal([]);
		};
			case _: fnormal([]);
		};
		};
			switch (f.cf_kind) {
			case Method(m): if (&&(=(mode, MSet), &&(<>(m, MethDynamic), !(ctx.untyped)))) {
			error("Cannot rebind this method : please use 'dynamic' before method declaration", p);
		} else {
			[];
		};
			switch ((new Tuple(ctx.curfun, e.eexpr))) {
			case (FunMemberAbstract | FunMemberAbstractLocal, TTypeExpr(TClassDecl({ cl_kind = KAbstractImpl(a) } = c))) if(&&(==(c, ctx.curclass), Meta.has(Meta.Impl, f.cf_meta))): var e = mk(TField(e, fmode), t, p);
			var ethis = get_this(ctx, p);
			var ethis = { (ethis) with etype = TAbstract(a, List.map(snd, a.a_params)) };
			AKUsing(e, ctx.curclass, f, ethis);
			case _: switch ((new Tuple(m, mode))) {
			case (MethInline, _): AKInline(e, f, fmode, t);
			case (MethMacro, MGet): display_error(ctx, "Macro functions must be called immediately", p);
			normal([]);
			case (MethMacro, MCall): AKMacro(e, f);
			case (_, MGet): var cmode = switch (fmode) {
			case FInstance(_, _, cf) | FStatic(_, cf) if(Meta.has(Meta.Generic, cf.cf_meta)): display_error(ctx, "Cannot create closure
			on generic function", p);
			fmode;
			case FInstance(c, tl, cf): FClosure(Some(c, tl), cf);
			case FStatic(_) | FEnum(_): fmode;
			case FAnon(f): FClosure(None, f);
			case FDynamic(_) | FClosure(_): assert False;
		};
			AKExpr(mk(TField(e, cmode), t, p));
			case _: normal([]);
		};
		};
			case Var(v): switch (switch (mode) {
			case MGet | MCall: v.v_read;
			case MSet: v.v_write;
		}) {
			case AccNo: switch (follow(e.etype)) {
			case TInst(c, _) if(||(is_parent(c, ctx.curclass), can_access(ctx, c, { (f) with cf_public = False }, False))): normal([]);
			case TAnon(a): switch (a.a_status.val) {
			case Opened if(=(mode, MSet)): f.cf_kind = Var({ (v) with v_write = AccNormal });
			normal([]);
			case Statics(c2) if(||(==(ctx.curclass, c2), can_access(ctx, c2, { (f) with cf_public = False }, True))): normal([]);
			case _: if (ctx.untyped) {
			normal([]);
		} else {
			AKNo(f.cf_name);
		};
		};
			case _: if (ctx.untyped) {
			normal([]);
		} else {
			AKNo(f.cf_name);
		};
		};
			case AccNormal: 	function is_maybe_method([]) return {
			switch ((new Tuple(v.v_write, follow(t), follow(e.etype)))) {
			case (AccNo | AccNever, TFun(_), TAnon(a)): switch (a.a_status.val) {
			case Statics(_) | EnumStatics(_): False;
			case _: True;
		};
			case _: False;
		};
		};
			if (&&(=(mode, MGet), is_maybe_method([]))) {
			AKExpr(mk(TField(e, FClosure(None, f)), t, p));
		} else {
			normal([]);
		};
			case AccCall: var m = ^(switch (mode) {
			case MSet: "set_";
			case _: "get_";
		}, f.cf_name);
			function is_abstract_this_access([]) return {
			switch ((new Tuple(e.eexpr, ctx.curfun))) {
			case (TTypeExpr(TClassDecl({ cl_kind = KAbstractImpl(_) } = c)), FunMemberAbstract | FunMemberAbstractLocal): ==(c, ctx.curclass);
			case _: False;
		};
		};
			if (&&(=(m, ctx.curfield.cf_name), switch (e.eexpr) {
			case TConst(TThis): True;
			case TTypeExpr(TClassDecl(c)) if(==(c, ctx.curclass)): True;
			case _: False;
		})) {
			var prefix = switch (ctx.com.platform) {
			case Flash if(Common.defined(ctx.com, Define.As3)): "$";
			case _: "";
		};
			if (is_extern_field(f)) {
			display_error(ctx, "This field cannot be accessed because it is not a real variable", p);
			display_error(ctx, "Add @: isVar here to enable it", f.cf_pos);
		} else {
			[];
		};
			AKExpr(mk(TField(e, if (=(prefix, "")) {
			fmode;
		} else {
			FDynamic(^(prefix, f.cf_name));
		}), t, p));
		} else {
			if (is_abstract_this_access([])) {
			var this = get_this(ctx, p);
			if (=(mode, MSet)) {
			var Tuple(c, a) = switch (ctx.curclass) {
			case { cl_kind = KAbstractImpl(a) } = c: (new Tuple(c, a));
			case _: assert False;
		};
			var f = PMap.find(m, c.cl_statics);
			var t = field_type(ctx, c, [], f, p);
			var ef = mk(TField(e, FStatic(c, f)), t, p);
			AKUsing(ef, c, f, this);
		} else {
			AKExpr(make_call(ctx, mk(TField(e, quick_field_dynamic(e.etype, m)), tfun(::(this.etype, []), t), p), ::(this, []), t, p));
		};
		} else {
			if (=(mode, MSet)) {
			AKSet(e, t, f);
		} else {
			AKExpr(make_call(ctx, mk(TField(e, quick_field_dynamic(e.etype, m)), tfun([], t), p), [], t, p));
		};
		};
		};
			case AccResolve: var fstring = mk(TConst(TString(f.cf_name)), ctx.t.tstring, p);
			var tresolve = tfun(::(ctx.t.tstring, []), t);
			AKExpr(make_call(ctx, mk(TField(e, FDynamic("resolve")), tresolve, p), ::(fstring, []), t, p));
			case AccNever: if (ctx.untyped) {
			normal([]);
		} else {
			AKNo(f.cf_name);
		};
			case AccInline: AKInline(e, f, fmode, t);
			case AccRequire(r, msg): switch (msg) {
			case None: error_require(r, p);
			case Some(msg): error(msg, p);
		};
		};
		};
		};

			public static function using_field(ctx, mode, e, i, p) return {
			if (=(mode, MSet)) {
			raise(Not_found);
		} else {
			[];
		};
			var is_dynamic = switch (follow(e.etype)) {
			case TMono(_): raise(Not_found);
			case t: ==(t, t_dynamic);
		};
			var check_constant_struct = ref(False);
			function loop(match) return switch (match) {
			case []: raise(Not_found);
			case ::(c, l): try {
			var cf = PMap.find(i, c.cl_statics);
			if (||(Meta.has(Meta.NoUsing, cf.cf_meta), !(can_access(ctx, c, cf, True)))) {
			raise(Not_found);
		} else {
			[];
		};
			var monos = List.map(function _: mk_mono([]), cf.cf_params);
			var map = apply_params(cf.cf_params, monos);
			var t = map(cf.cf_type);
			switch (follow(t)) {
			case TFun(::((_, _, TType({ t_path = (::(haxe, ::(macro, [])), ExprOf) }, ::(t0, [])) | t0), args), r): if (&&(is_dynamic, !=(follow(t0), t_dynamic))) {
			raise(Not_found);
		} else {
			[];
		};
			var e = Codegen.AbstractCast.cast_or_unify_raise(ctx, t0, e, p);
			List.iter2(function m: function (name, t): switch (follow(t)) {
			case TInst({ cl_kind = KTypeParameter(constr) }, _) if(&&(<>(constr, []), !(has_mono(m)))): List.iter(function tc: Type.unify(m, map(tc)), constr);
			case _: [];
		}, monos, cf.cf_params);
			var et = type_module_type(ctx, TClassDecl(c), None, p);
			AKUsing(mk(TField(et, FStatic(c, cf)), t, p), c, cf, e);
			case _: raise(Not_found);
		};
		} catch(e:T) {
			McOr(McArr(PaId(IdUid(Not_found)),ExNil,ExApp(ExId(IdLid(<...>)),ExId(IdLid(<...>)))),McArr(PaOrp(PaApp(PaId(<...>),PaId(<...>)),PaApp(PaApp(<...>,<...>),PaAny)),ExNil,ExSeq(ExSem(ExIfe(<...>,<...>,<...>),ExApp(<...>,<...>)))))					case Not_found: loop(l);
			case Unify_error(el) | Error(Unify(el), _): if (List.exists(function case Has_extra_field(_): True;
			case _: False, el)) {
			check_constant_struct.val = True;
		} else {
			[];
		};
			loop(l);
		};
		};
			try {
			loop(ctx.m.module_using);
		} catch(e:Not_found) {
			try {
			var acc = loop(ctx.g.global_using);
			switch (acc) {
			case AKUsing(_, c, _, _): add_dependency(ctx.m.curmod, c.cl_module);
			case _: assert False;
		};
			acc;
		} catch(e:Not_found) {
			if (!(check_constant_struct.val)) {
			raise(Not_found);
		} else {
			[];
		};
			remove_constant_flag(e.etype, function ok: if (ok) {
			using_field(ctx, mode, e, i, p);
		} else {
			raise(Not_found);
		});
		};
		};
		};

			public static function type_ident_raise(?:(imported_enums = True), ctx, i, p, mode) return {
			switch (i) {
			case true: if (=(mode, MGet)) {
			AKExpr(mk(TConst(TBool(True)), ctx.t.tbool, p));
		} else {
			AKNo(i);
		};
			case false: if (=(mode, MGet)) {
			AKExpr(mk(TConst(TBool(False)), ctx.t.tbool, p));
		} else {
			AKNo(i);
		};
			case this: switch ((new Tuple(mode, ctx.curclass.cl_kind))) {
			case (MSet, KAbstractImpl(_)): switch (ctx.curfield.cf_kind) {
			case Method(MethInline): [];
			case Method(_) if(=(ctx.curfield.cf_name, "_new")): [];
			case _: error("You can only modify 'this' inside an inline function", p);
		};
			AKExpr(get_this(ctx, p));
			case (MCall, KAbstractImpl(_)) | (MGet, _): AKExpr(get_this(ctx, p));
			case _: AKNo(i);
		};
			case super: var t = switch (ctx.curclass.cl_super) {
			case None: error("Current class does not have a superclass", p);
				case Some(c, params): TInst(c, params);
			};
				switch (ctx.curfun) {
				case FunMember | FunConstructor: [];
				case FunMemberAbstract: error("Cannot access super inside an abstract function", p);
				case FunStatic: error("Cannot access super inside a static function", p);
				case FunMemberClassLocal | FunMemberAbstractLocal: error("Cannot access super inside a local function", p);
			};
				if (&&(<>(mode, MSet), ctx.in_super_call)) {
				ctx.in_super_call = False;
			} else {
				[];
			};
				AKExpr(mk(TConst(TSuper), t, p));
				case null: if (=(mode, MGet)) {
				AKExpr(null(mk_mono([]), p));
			} else {
				AKNo(i);
			};
				case _: try {
				var v = PMap.find(i, ctx.locals);
				switch (v.v_extra) {
				case Some(params, e): var t = monomorphs(params, v.v_type);
				switch (e) {
				case Some({ eexpr = TFunction(f) } = e): switch (mode) {
				case MSet: error("Cannot set inline closure", p);
				case MGet: error("Cannot create closure on inline closure", p);
				case MCall: var c = mk_class(ctx.m.curmod, (new Tuple(::("local", []), v.v_name)), e.epos);
				var cf = { (mk_field(v.v_name, v.v_type, e.epos)) with cf_params = params;
				cf_expr = Some(e);
				cf_kind = Method(MethInline) };
				c.cl_extern = True;
				c.cl_fields = PMap.add(cf.cf_name, cf, PMap.empty);
				AKInline(mk(TConst(TNull), TInst(c, []), p), cf, FInstance(c, [], cf), t);
			};
				case _: AKExpr(mk(TLocal(v), t, p));
			};
				case _: AKExpr(mk(TLocal(v), v.v_type, p));
			};
			} catch(e:Not_found) {
				try {
				if (=(ctx.curfun, FunStatic)) {
				raise(Not_found);
			} else {
				[];
			};
				var Tuple(c, t, f) = class_field(ctx, ctx.curclass, List.map(snd, ctx.curclass.cl_params), i, p);
				field_access(ctx, mode, f, switch (c) {
				case None: FAnon(f);
				case Some(c, tl): FInstance(c, tl, f);
			}, t, get_this(ctx, p), p);
			} catch(e:Not_found) {
				try {
				if (=(ctx.curfun, FunStatic)) {
				raise(Not_found);
			} else {
				[];
			};
				switch (using_field(ctx, mode, mk(TConst(TThis), ctx.tthis, p), i, p)) {
				case AKUsing(et, c, f, _): AKUsing(et, c, f, get_this(ctx, p));
				case _: assert False;
			};
			} catch(e:Not_found) {
				try {
				var f = PMap.find(i, ctx.curclass.cl_statics);
				var e = type_type(ctx, ctx.curclass.cl_path, p);
				field_access(ctx, mode, f, FStatic(ctx.curclass, f), field_type(ctx, ctx.curclass, [], f, p), e, p);
			} catch(e:Not_found) {
				try {
				if (!(imported_enums)) {
				raise(Not_found);
			} else {
				[];
			};
				function wrap(e) return {
				if (=(mode, MSet)) {
				AKNo(i);
			} else {
				AKExpr(e);
			};
			};
				function loop(l) return {
				switch (l) {
				case []: raise(Not_found);
				case ::(t, l): switch (t) {
				case TAbstractDecl({ a_impl = Some(c) } = a) if(Meta.has(Meta.Enum, a.a_meta)): try {
				var cf = PMap.find(i, c.cl_statics);
				if (!(Meta.has(Meta.Enum, cf.cf_meta))) {
				loop(l);
			} else {
				var et = type_module_type(ctx, TClassDecl(c), None, p);
				AKInline(et, cf, FStatic(c, cf), monomorphs(cf.cf_params, cf.cf_type));
			};
			} catch(e:Not_found) {
				loop(l);
			};
				case TClassDecl(_) | TAbstractDecl(_): loop(l);
				case TTypeDecl(t): switch (follow(t.t_type)) {
				case TEnum(e, _): loop(::(TEnumDecl(e), l));
				case _: loop(l);
			};
				case TEnumDecl(e): try {
				var ef = PMap.find(i, e.e_constrs);
				var et = type_module_type(ctx, t, None, p);
				var monos = List.map(function _: mk_mono([]), e.e_params);
				var monos2 = List.map(function _: mk_mono([]), ef.ef_params);
				wrap(mk(TField(et, FEnum(e, ef)), enum_field_type(ctx, e, ef, monos, monos2, p), p));
			} catch(e:Not_found) {
				loop(l);
			};
			};
			};
			};
				try {
				loop(List.rev(ctx.m.curmod.m_types));
			} catch(e:Not_found) {
				loop(ctx.m.module_types);
			};
			} catch(e:Not_found) {
				var Tuple(t, name) = PMap.find(i, ctx.m.module_globals);
				var e = type_module_type(ctx, t, None, p);
				type_field(ctx, e, name, p, mode);
			};
			};
			};
			};
			};
			};
			};

				public static function type_field(?:(resume = False), ctx, e, i, p, mode) return {
				function no_field([]) return {
				if (resume) {
				raise(Not_found);
			} else {
				[];
			};
				var t = switch (follow(e.etype)) {
				case TAnon(a): switch (a.a_status.val) {
				case Statics({ cl_kind = KAbstractImpl(a) }): TAbstract(a, []);
				case _: e.etype;
			};
				case TInst({ cl_kind = KAbstractImpl(a) }, _): TAbstract(a, []);
				case _: e.etype;
			};
				function has_special_field(a) return {
				||(List.exists(function (_, cf): =(cf.cf_name, i), a.a_ops), ||(List.exists(function (_, _, cf): =(cf.cf_name, i), a.a_unops), List.exists(function cf: =(cf.cf_name, i), a.a_array)));
			};
				if (!(ctx.untyped)) {
				switch (t) {
				case TAbstract(a, _) if(has_special_field(a)): display_error(ctx, ^("Field ", ^(i, " cannot be called directly because it
				has no expression")), p);
				case _: display_error(ctx, string_error(i, string_source(t), ^(s_type(print_context([]), t), ^(" has no field ", i))), p);
			};
			} else {
				[];
			};
				AKExpr(mk(TField(e, FDynamic(i)), mk_mono([]), p));
			};
				switch (follow(e.etype)) {
				case TInst(c, params): 	function loop_dyn(c, params) return {
				switch (c.cl_dynamic) {
				case Some(t): var t = apply_params(c.cl_params, params, t);
				if (&&(||(=(mode, MGet), =(mode, MCall)), PMap.mem("resolve", c.cl_fields))) {
				var f = PMap.find("resolve", c.cl_fields);
				switch (f.cf_kind) {
				case Method(MethMacro): display_error(ctx, "The macro accessor is not allowed for field resolve", f.cf_pos);
				case _: [];
			};
				var texpect = tfun(::(ctx.t.tstring, []), t);
				var tfield = apply_params(c.cl_params, params, monomorphs(f.cf_params, f.cf_type));
				try {
				Type.unify(tfield, texpect);
			} catch(e:Unify_error(l)) {
				display_error(ctx, "Field resolve has an invalid type", f.cf_pos);
				display_error(ctx, error_msg(Unify(::(Cannot_unify(tfield, texpect), []))), f.cf_pos);
			};
				AKExpr(make_call(ctx, mk(TField(e, FInstance(c, params, f)), tfield, p), ::(Codegen.type_constant(ctx.com, String(i), p), []), t, p));
			} else {
				AKExpr(mk(TField(e, FDynamic(i)), t, p));
			};
				case None: switch (c.cl_super) {
				case None: raise(Not_found);
				case Some(c, params): loop_dyn(c, params);
			};
			};
			};
				try {
				var Tuple(c2, t, f) = class_field(ctx, c, params, i, p);
				if (=(e.eexpr, TConst(TSuper))) {
				switch ((new Tuple(mode, f.cf_kind))) {
				case (MGet, Var({ v_read = AccCall })) | (MSet, Var({ v_write = AccCall })) | (MCall, Var({ v_read = AccCall })): [];
				case (MCall, Var(_)): display_error(ctx, "Cannot access superclass variable for calling : needs to be a proper method", p);
					case (MCall, _): [];
					case (MGet, Var(_)) | (MSet, Var(_)) if(switch (c2) {
					case Some({ cl_extern = True; cl_path = (::(flash, _), _) }, _): True;
					case _: False;
				}): [];
					case (_, Method(_)): display_error(ctx, "Cannot create closure on super method", p);
					case _: display_error(ctx, "Normal variables cannot be accessed with 'super', use 'this' instead", p);
				};
				} else {
					[];
				};
					if (&&(!(can_access(ctx, c, f, False)), !(ctx.untyped))) {
					display_error(ctx, ^("Cannot access private field ", i), p);
				} else {
					[];
				};
					field_access(ctx, mode, f, switch (c2) {
					case None: FAnon(f);
					case Some(c, tl): FInstance(c, tl, f);
				}, apply_params(c.cl_params, params, t), e, p);
				} catch(e:Not_found) {
					try {
					using_field(ctx, mode, e, i, p);
				} catch(e:Not_found) {
					try {
					loop_dyn(c, params);
				} catch(e:Not_found) {
					try {
					switch (c.cl_kind) {
					case KTypeParameter(tl): 	function loop(tl) return {
					switch (tl) {
					case ::(t, tl): switch (follow(t)) {
					case TAbstract({ a_impl = Some(c) }, tl) if(PMap.mem(i, c.cl_statics)): var e = mk_cast(e, t, p);
					type_field(ctx, e, i, p, mode);
					case _: loop(tl);
				};
					case []: raise(Not_found);
				};
				};
					loop(tl);
					case _: raise(Not_found);
				};
				} catch(e:Not_found) {
					if (PMap.mem(i, c.cl_statics)) {
					error(^("Cannot access static field ", ^(i, " from a class instance")), p);
					} else {
						[];
					};
						no_field([]);
					};
					};
					};
					};
						case TDynamic(t): try {
						using_field(ctx, mode, e, i, p);
					} catch(e:Not_found) {
						AKExpr(mk(TField(e, FDynamic(i)), t, p));
					};
						case TAnon(a): try {
						var f = PMap.find(i, a.a_fields);
						if (&&(!(f.cf_public), !(ctx.untyped))) {
						switch (a.a_status.val) {
						case Closed | Extend(_): [];
						case Statics(c) if(can_access(ctx, c, f, True)): [];
						case _: display_error(ctx, ^("Cannot access private field ", i), p);
					};
					} else {
						[];
					};
						var Tuple(fmode, ft) = switch (a.a_status.val) {
						case Statics(c): (new Tuple(FStatic(c, f), field_type(ctx, c, [], f, p)));
						case EnumStatics(e): (new Tuple(FEnum(e, try {
						PMap.find(f.cf_name, e.e_constrs);
					} catch(e:Not_found) {
						assert False;
					}), Type.field_type(f)));
						case _: switch (f.cf_params) {
						case []: (new Tuple(FAnon(f), Type.field_type(f)));
						case l: var monos = List.map(function _: mk_mono([]), l);
						var t = apply_params(f.cf_params, monos, f.cf_type);
						add_constraint_checks(ctx, [], [], f, monos, p);
						(new Tuple(FAnon(f), t));
					};
					};
						field_access(ctx, mode, f, fmode, ft, e, p);
					} catch(e:Not_found) {
						if (is_closed(a)) {
						try {
						using_field(ctx, mode, e, i, p);
					} catch(e:Not_found) {
						no_field([]);
					};
					} else {
						var f = { () with cf_name = i;
						cf_type = mk_mono([]);
						cf_doc = None;
						cf_meta = no_meta;
						cf_public = True;
						cf_pos = p;
						cf_kind = Var({ () with v_read = AccNormal;
						v_write = switch (mode) {
						case MSet: AccNormal;
						case MGet | MCall: AccNo;
					} });
						cf_expr = None;
						cf_params = [];
						cf_overloads = [] };
						a.a_fields = PMap.add(i, f, a.a_fields);
						field_access(ctx, mode, f, FAnon(f), Type.field_type(f), e, p);
					};
					};
						case TMono(r): var f = { () with cf_name = i;
						cf_type = mk_mono([]);
						cf_doc = None;
						cf_meta = no_meta;
						cf_public = True;
						cf_pos = p;
						cf_kind = Var({ () with v_read = AccNormal;
						v_write = switch (mode) {
						case MSet: AccNormal;
						case MGet | MCall: AccNo;
					} });
						cf_expr = None;
						cf_params = [];
						cf_overloads = [] };
						var x = ref(Opened);
						var t = TAnon({ () with a_fields = PMap.add(i, f, PMap.empty);
						a_status = x });
						ctx.opened = ::(x, ctx.opened);
						r.val = Some(t);
						field_access(ctx, mode, f, FAnon(f), Type.field_type(f), e, p);
						case TAbstract(a, pl): var static_abstract_access_through_instance = ref(False);
						try {
						var c = switch (a.a_impl) {
						case None: raise(Not_found);
						case Some(c): c;
					};
						var f = PMap.find(i, c.cl_statics);
						if (&&(!(can_access(ctx, c, f, True)), !(ctx.untyped))) {
						display_error(ctx, ^("Cannot access private field ", i), p);
					} else {
						[];
					};
						function field_type(f) return {
						if (!(Meta.has(Meta.Impl, f.cf_meta))) {
						static_abstract_access_through_instance.val = True;
						raise(Not_found);
					} else {
						[];
					};
						var t = field_type(ctx, c, [], f, p);
						apply_params(a.a_params, pl, t);
					};
						var et = type_module_type(ctx, TClassDecl(c), None, p);
						function field_expr(f, t) return {
						mk(TField(et, FStatic(c, f)), t, p);
					};
						switch ((new Tuple(mode, f.cf_kind))) {
						case (MGet | MCall, Var({ v_read = AccCall })): var f = PMap.find(^("get_", f.cf_name), c.cl_statics);
						var t = field_type(f);
						var r = switch (follow(t)) {
						case TFun(_, r): r;
						case _: raise(Not_found);
					};
						var ef = field_expr(f, t);
						AKExpr(make_call(ctx, ef, ::(e, []), r, p));
						case (MSet, Var({ v_write = AccCall })): var f = PMap.find(^("set_", f.cf_name), c.cl_statics);
						var t = field_type(f);
						var ef = field_expr(f, t);
						AKUsing(ef, c, f, e);
						case (MGet | MCall, Var({ v_read = AccNever })): AKNo(f.cf_name);
						case (MGet | MCall, _): 	function loop(cfl) return {
						switch (cfl) {
						case []: error(Printf.sprintf("Field % s cannot be called on % s", f.cf_name, s_type(print_context([]), e.etype)), p);
						case ::(cf, cfl): switch (follow(apply_params(a.a_params, pl, monomorphs(cf.cf_params, cf.cf_type)))) {
						case TFun(::((_, _, t1), _), _) if(type_iseq(t1, Abstract.get_underlying_type(a, pl))): cf;
						case _: loop(cfl);
					};
					};
					};
						var f = switch (f.cf_overloads) {
						case []: f;
						case cfl: loop(::(f, cfl));
					};
						var t = field_type(f);
						switch (follow(t)) {
						case TFun(::((_, _, t1), _), _): [];
						case _: error(^("Invalid call to static function ", ^(i, " through abstract instance")), p);
					};
						var ef = field_expr(f, t);
						AKUsing(ef, c, f, e);
						case (MSet, _): error("This operation is unsupported", p);
					};
					} catch(e:Not_found) {
						try {
						var Tuple(_, el, _) = Meta.get(Meta.Forward, a.a_meta);
						if (&&(!(List.exists(function e: switch (fst(e)) {
						case EConst(Ident(s) | String(s)): =(s, i);
						case _: error("Identifier or string expected as argument to @ : forward", pos(e));
					}, el)), <>(el, []))) {
						raise(Not_found);
					} else {
						[];
					};
						type_field(ctx, { (e) with etype = apply_params(a.a_params, pl, a.a_this) }, i, p, mode);
					} catch(e:Not_found) {
						try {
						using_field(ctx, mode, e, i, p);
					} catch(e:Not_found) {
						try {
						switch ((new Tuple(ctx.curfun, e.eexpr))) {
						case (FunMemberAbstract, TConst(TThis)): type_field(ctx, { (e) with etype = apply_params(a.a_params, pl, a.a_this) }, i, p, mode);
						case _: raise(Not_found);
					};
					} catch(e:Not_found) {
						try {
						var Tuple(c, cf) = switch ((new Tuple(a.a_impl, a.a_resolve))) {
						case (Some(c), Some(cf)): (new Tuple(c, cf));
						case _: raise(Not_found);
					};
						var et = type_module_type(ctx, TClassDecl(c), None, p);
						var t = apply_params(a.a_params, pl, field_type(ctx, c, [], cf, p));
						var ef = mk(TField(et, FStatic(c, cf)), t, p);
						AKExpr(build_call_ref.val(ctx, AKUsing(ef, c, cf, e), ::((new Tuple(EConst(String(i)), p)), []), NoValue, p));
					} catch(e:Not_found) {
						if (static_abstract_access_through_instance.val) {
						error(^("Invalid call to static function ", ^(i, " through abstract instance")), p);
					} else {
						no_field([]);
					};
					};
					};
					};
					};
					};
						case _: try {
						using_field(ctx, mode, e, i, p);
					} catch(e:Not_found) {
						no_field([]);
					};
					};
					};

						public static function type_bind(ctx, etexpr, params, p) return {
						var Tuple(args, ret) = switch (follow(e.etype)) {
						case TFun(args, ret): (new Tuple(args, ret));
						case _: error("First parameter of callback is not a function", p);
					};
						function vexpr(v) return {
						mk(TLocal(v), v.v_type, p);
					};
						var acount = ref(0);
						function alloc_name(n) return {
						if (||(=(n, ""), >(String.length(n), 2))) {
						incr(acount);
						^("a", string_of_int(acount.val));
					} else {
						n;
					};
					};
						function loop(args, params, given_args, missing_args, ordered_args) return {
						switch ((new Tuple(args, params))) {
						case ([], []): (new Tuple(given_args, missing_args, ordered_args));
						case ([], _): error("Too many callback arguments", p);
						case (::((n, o, t), args), []) if(o): var a = if (is_pos_infos(t)) {
						var infos = mk_infos(ctx, p, []);
						@(ordered_args, ::(type_expr(ctx, infos, WithType(t)), []));
					} else {
						if (ctx.com.config.pf_pad_nulls) {
						@(ordered_args, ::(mk(TConst(TNull), t_dynamic, p), []));
					} else {
						ordered_args;
					};
					};
						loop(args, [], given_args, missing_args, a);
						case (::((n, o, t), _), ::((EConst(Ident(_)), p), _)) if(&&(!(ctx.com.config.pf_can_skip_non_nullable_argument), &&(o, !(is_nullable(t))))): error("Usage
						of _ is not supported for optional non - nullable arguments", p);
							case (::((n, o, t), args), [] = params) | (::((n, o, t), args), ::((EConst(Ident(_)), _), params)): var v = alloc_var(alloc_name(n), if (o) {
							ctx.t.tnull(t);
						} else {
							t;
						});
							loop(args, params, given_args, @(missing_args, ::((new Tuple(v, o)), [])), @(ordered_args, ::(vexpr(v), [])));
							case (::((n, o, t), args), ::(param, params)): var e = type_expr(ctx, param, WithType(t));
							unify(ctx, e.etype, t, p);
							var v = alloc_var(alloc_name(n), t);
							loop(args, params, @(given_args, ::((new Tuple(v, o, Some(e))), [])), missing_args, @(ordered_args, ::(vexpr(v), [])));
						};
						};
							var Tuple(given_args, missing_args, ordered_args) = loop(args, params, [], [], []);
							function gen_loc_name(n) return {
							var name = if (=(n, 0)) {
							"f";
						} else {
							^("f", string_of_int(n));
						};
							if (List.exists(function (n, _, _): =(name, n), args)) {
							gen_loc_name(+(n, 1));
						} else {
							name;
						};
						};
							var loc = alloc_var(gen_loc_name(0), e.etype);
							var given_args = ::((new Tuple(loc, False, Some(e))), given_args);
							function inner_fun_args(l) return {
							List.map(function (v, o): (new Tuple(v.v_name, o, v.v_type)), l);
						};
							var t_inner = TFun(inner_fun_args(missing_args), ret);
							var call = make_call(ctx, vexpr(loc), ordered_args, ret, p);
							var e_ret = switch (follow(ret)) {
							case TAbstract({ a_path = ([], Void) }, _): call;
							case TMono(_): mk(TReturn(Some(call)), t_dynamic, p);
							case _: mk(TReturn(Some(call)), t_dynamic, p);
						};
							var func = mk(TFunction({ () with tf_args = List.map(function (v, o): (new Tuple(v, if (o) {
							Some(TNull);
						} else {
							None;
						})), missing_args);
							tf_type = ret;
							tf_expr = e_ret }), t_inner, p);
							function outer_fun_args(l) return {
							List.map(function (v, o, _): (new Tuple(v.v_name, o, v.v_type)), l);
						};
							var func = mk(TFunction({ () with tf_args = List.map(function (v, _, _): (new Tuple(v, None)), given_args);
							tf_type = t_inner;
							tf_expr = mk(TReturn(Some(func)), t_inner, p) }), TFun(outer_fun_args(given_args), t_inner), p);
							make_call(ctx, func, List.map(function (_, _, e): switch (e) {
							case Some(e): e;
							case None: assert False;
						}, given_args), t_inner, p);
						};

							public static function unify_int(ctx, e, k) return {
							function is_dynamic(t) return {
							switch (follow(t)) {
							case TDynamic(_): True;
							case _: False;
						};
						};
							function is_dynamic_array(t) return {
							switch (follow(t)) {
							case TInst(_, ::(p, [])): is_dynamic(p);
							case _: True;
						};
						};
							function is_dynamic_field(t, f) return {
							switch (follow(t)) {
							case TAnon(a): try {
							is_dynamic(PMap.find(f, a.a_fields).cf_type);
						} catch(e:Not_found) {
							False;
						};
							case TInst(c, tl): try {
							is_dynamic(apply_params(c.cl_params, tl, var Tuple(_, t, _) = Type.class_field(c, tl, f);
							t));
						} catch(e:Not_found) {
							False;
						};
							case _: True;
						};
						};
							function is_dynamic_return(t) return {
							switch (follow(t)) {
							case TFun(_, r): is_dynamic(r);
							case _: True;
						};
						};
							function maybe_dynamic_mono(e) return {
							switch (e.eexpr) {
							case TLocal(_): is_dynamic(e.etype);
							case TArray({ etype = t } = e, _): ||(is_dynamic_array(t), maybe_dynamic_rec(e, t));
							case TField({ etype = t } = e, f): ||(is_dynamic_field(t, field_name(f)), maybe_dynamic_rec(e, t));
							case TCall({ etype = t } = e, _): ||(is_dynamic_return(t), maybe_dynamic_rec(e, t));
							case TParenthesis(e) | TMeta(_, e): maybe_dynamic_mono(e);
							case TIf(_, a, Some(b)): ||(maybe_dynamic_mono(a), maybe_dynamic_mono(b));
							case _: False;
						};
						};
							function maybe_dynamic_rec(e, t) return {
							switch (follow(t)) {
							case TMono(_) | TDynamic(_): maybe_dynamic_mono(e);
							case TAnon(a) if(=(a.a_status.val, Opened)): maybe_dynamic_mono(e);
							case _: False;
						};
						};
							switch (k) {
							case KUnk | KDyn if(maybe_dynamic_mono(e)): unify(ctx, e.etype, ctx.t.tfloat, e.epos);
							False;
							case _: unify(ctx, e.etype, ctx.t.tint, e.epos);
							True;
						};
						};

							public static function type_generic_function(ctx, Tuple(e, fa), el, ?:(using_param = None), with_type, p) return {
							var Tuple(c, tl, cf, stat) = switch (fa) {
							case FInstance(c, tl, cf): (new Tuple(c, tl, cf, False));
							case FStatic(c, cf): (new Tuple(c, [], cf, True));
							case _: assert False;
						};
							if (=(cf.cf_params, [])) {
							error("Function has no type parameters and cannot be generic", p);
						} else {
							[];
						};
							var monos = List.map(function _: mk_mono([]), cf.cf_params);
							function map(t) return {
							apply_params(cf.cf_params, monos, t);
						};
							function map(t) return {
							if (stat) {
							map(t);
						} else {
							apply_params(c.cl_params, tl, map(t));
						};
						};
							var t = map(cf.cf_type);
							var Tuple(args, ret) = switch ((new Tuple(t, using_param))) {
							case (TFun(::((_, _, ta), args), ret), Some(e)): var ta = if (!(Meta.has(Meta.Impl, cf.cf_meta))) {
							ta;
						} else {
							switch (follow(ta)) {
							case TAbstract(a, tl): Abstract.get_underlying_type(a, tl);
							case _: assert False;
						};
						};
							unify(ctx, e.etype, ta, p);
							(new Tuple(args, ret));
							case (TFun(args, ret), None): (new Tuple(args, ret));
							case _: error("Invalid field type for generic call", p);
							};
								switch (with_type) {
								case WithType(t): unify(ctx, ret, t, p);
								case WithTypeResume(t): try {
								unify_raise(ctx, ret, t, p);
							} catch(e:Error(Unify(l))(_)) {
								raise(WithTypeError(l, p));
							};
								case _: [];
							};
								var Tuple(el, _) = unify_call_args(ctx, el, args, ret, p, False, False);
								try {
								check_constraints(ctx, cf.cf_name, cf.cf_params, monos, map, False, p);
							} catch(e:Unify_error(l)) {
								display_error(ctx, error_msg(Unify(l)), p);
							};
								var el = switch (using_param) {
								case None: el;
								case Some(e): ::(e, el);
							};
								try {
								var gctx = Codegen.make_generic(ctx, cf.cf_params, monos, p);
								var name = ^(cf.cf_name, ^("_", gctx.Codegen.name));
								function unify_existing_field(tcf, pcf) return {
								try {
								unify_raise(ctx, tcf, t, p);
							} catch(e:Error(Unify(_))(_) = err) {
								display_error(ctx, ^("Cannot create field ", ^(name, " due to type mismatch")), p);
								display_error(ctx, "Conflicting field was defined here", pcf);
								raise(err);
							};
							};
								var cf2 = try {
								var cf2 = if (stat) {
								var cf2 = PMap.find(name, c.cl_statics);
								unify_existing_field(cf2.cf_type, cf2.cf_pos);
								cf2;
							} else {
								var cf2 = PMap.find(name, c.cl_fields);
								unify_existing_field(cf2.cf_type, cf2.cf_pos);
								cf2;
							};
								cf2;
							} catch(e:Not_found) {
								var cf2 = mk_field(name, t, cf.cf_pos);
								if (stat) {
								c.cl_statics = PMap.add(name, cf2, c.cl_statics);
								c.cl_ordered_statics = ::(cf2, c.cl_ordered_statics);
							} else {
								if (List.memq(cf, c.cl_overrides)) {
								c.cl_overrides = ::(cf2, c.cl_overrides);
							} else {
								[];
							};
								c.cl_fields = PMap.add(name, cf2, c.cl_fields);
								c.cl_ordered_fields = ::(cf2, c.cl_ordered_fields);
							};
								ignore(follow(cf.cf_type));
								cf2.cf_expr = switch (cf.cf_expr) {
								case None: error("Recursive @ : generic function", p);
								case Some(e): Some(Codegen.generic_substitute_expr(gctx, e));
							};
								cf2.cf_kind = cf.cf_kind;
								cf2.cf_public = cf.cf_public;
								var metadata = List.filter(function (m, _, _): switch (m) {
								case Meta.Generic: False;
								case _: True;
							}, cf.cf_meta);
								cf2.cf_meta = ::((new Tuple(Meta.NoCompletion, [], p)), ::((new Tuple(Meta.NoUsing, [], p)), ::((new Tuple(Meta.GenericInstance, [], p)), metadata)));
								cf2;
							};
								var e = if (stat) {
								type_type(ctx, c.cl_path, p);
							} else {
								e;
							};
								var fa = if (stat) {
								FStatic(c, cf2);
							} else {
								FInstance(c, tl, cf2);
							};
								var e = mk(TField(e, fa), cf2.cf_type, p);
								make_call(ctx, e, el, ret, p);
							} catch(e:Codegen.Generic_Exception(msg)(p)) {
								error(msg, p);
							};
							};

								public static function call_to_string(ctx, c, e) return {
								var et = type_module_type(ctx, TClassDecl(c), None, e.epos);
								var cf = PMap.find("toString", c.cl_statics);
								make_call(ctx, mk(TField(et, FStatic(c, cf)), cf.cf_type, e.epos), ::(e, []), ctx.t.tstring, e.epos);
							};

								public static function type_binop(ctx, op, e1, e2, is_assign_op, with_type, p) return {
								switch (op) {
								case OpAssign: var e1 = type_access(ctx, fst(e1), snd(e1), MSet);
								var tt = switch (e1) {
								case AKNo(_) | AKInline(_) | AKUsing(_) | AKMacro(_) | AKAccess(_): Value;
								case AKSet(_, t, _): WithType(t);
								case AKExpr(e): WithType(e.etype);
							};
								var e2 = type_expr(ctx, e2, tt);
								switch (e1) {
								case AKNo(s): error(^("Cannot access field or identifier ", ^(s, " for writing")), p);
									case AKExpr(e1): var e2 = Codegen.AbstractCast.cast_or_unify(ctx, e1.etype, e2, p);
									check_assign(ctx, e1);
									switch ((new Tuple(e1.eexpr, e2.eexpr))) {
									case (TLocal(i1), TLocal(i2)) if(==(i1, i2)): error("Assigning a value to itself", p);
									case (TField({ eexpr = TConst(TThis) }, FInstance(_, _, f1)), TField({ eexpr = TConst(TThis) }, FInstance(_, _, f2))) if(==(f1, f2)): error("Assigning
									a value to itself", p);
									case (_, _): [];
								};
									mk(TBinop(op, e1, e2), e1.etype, p);
									case AKSet(e, t, cf): var e2 = Codegen.AbstractCast.cast_or_unify(ctx, t, e2, p);
									make_call(ctx, mk(TField(e, quick_field_dynamic(e.etype, ^("set_", cf.cf_name))), tfun(::(t, []), t), p), ::(e2, []), t, p);
									case AKAccess(a, tl, c, ebase, ekey): mk_array_set_call(ctx, Codegen.AbstractCast.find_array_access(ctx, a, tl, ekey, Some(e2), p), c, ebase, p);
									case AKUsing(ef, _, _, et): var ret = switch (follow(ef.etype)) {
									case TFun(::(_, ::((_, _, t), [])), ret): unify(ctx, e2.etype, t, p);
									ret;
									case _: error("Invalid field type for abstract setter", p);
									};
										make_call(ctx, ef, ::(et, ::(e2, [])), ret, p);
										case AKInline(_) | AKMacro(_): assert False;
									};
										case OpAssignOp(op): switch (type_access(ctx, fst(e1), snd(e1), MSet)) {
										case AKNo(s): error(^("Cannot access field or identifier ", ^(s, " for writing")), p);
											case AKExpr(e): var eop = type_binop(ctx, op, e1, e2, True, with_type, p);
											switch (eop.eexpr) {
											case TBinop(_, _, e2): unify(ctx, eop.etype, e.etype, p);
											check_assign(ctx, e);
											mk(TBinop(OpAssignOp(op), e, e2), e.etype, p);
											case TMeta((Meta.RequiresAssign, _, _), e2): unify(ctx, e2.etype, e.etype, p);
											check_assign(ctx, e);
											mk(TBinop(OpAssign, e, e2), e.etype, p);
											case _: check_assign(ctx, e);
											eop;
										};
											case AKSet(e, t, cf): var l = save_locals(ctx);
											var v = gen_local(ctx, e.etype);
											var ev = mk(TLocal(v), e.etype, p);
											var get = type_binop(ctx, op, (new Tuple(EField((new Tuple(EConst(Ident(v.v_name)), p)), cf.cf_name), p)), e2, True, with_type, p);
											var e' = switch (get.eexpr) {
											case TBinop(_) | TMeta((Meta.RequiresAssign, _, _), _): unify(ctx, get.etype, t, p);
											make_call(ctx, mk(TField(ev, quick_field_dynamic(ev.etype, ^("set_", cf.cf_name))), tfun(::(t, []), t), p), ::(get, []), t, p);
											case _: get;
										};
											l([]);
											mk(TBlock(::(mk(TVar(v, Some(e)), ctx.t.tvoid, p), ::(e', []))), t, p);
											case AKUsing(ef, c, cf, et): var ta = switch (c.cl_kind) {
											case KAbstractImpl(a): TAbstract(a, List.map(function _: mk_mono([]), a.a_params));
											case _: assert False;
										};
											var ret = switch (follow(ef.etype)) {
											case TFun(::(_, ::(_, [])), ret): ret;
											case _: error("Invalid field type for abstract setter", p);
											};
												var l = save_locals(ctx);
												var Tuple(v, is_temp) = switch (et.eexpr) {
												case TLocal(v) if(!(=(v.v_name, "this"))): (new Tuple(v, False));
												case _: (new Tuple(gen_local(ctx, ta), True));
											};
												var ev = mk(TLocal(v), ta, p);
												var getter_name = String.sub(cf.cf_name, 4, -(String.length(cf.cf_name), 4));
												var get = type_binop(ctx, op, (new Tuple(EField((new Tuple(EConst(Ident(v.v_name)), p)), getter_name), p)), e2, True, with_type, p);
												unify(ctx, get.etype, ret, p);
												l([]);
												var e_call = make_call(ctx, ef, ::(ev, ::(get, [])), ret, p);
												if (is_temp) {
												mk(TBlock(::(mk(TVar(v, Some(et)), ctx.t.tvoid, p), ::(e_call, []))), ret, p);
											} else {
												e_call;
											};
												case AKAccess(a, tl, c, ebase, ekey): var Tuple(cf_get, tf_get, r_get, ekey, _) = Codegen.AbstractCast.find_array_access(ctx, a, tl, ekey, None, p);
												var Tuple(ekey, l) = switch (Optimizer.make_constant_expression(ctx, ekey)) {
												case Some(e): (new Tuple(e, function []: None));
												case None: var save = save_locals(ctx);
												var v = gen_local(ctx, ekey.etype);
												var e = mk(TLocal(v), ekey.etype, p);
												(new Tuple(e, function []: save([]);
												Some(mk(TVar(v, Some(ekey)), ctx.t.tvoid, p))));
											};
												var eget = mk_array_get_call(ctx, (new Tuple(cf_get, tf_get, r_get, ekey, None)), c, ebase, p);
												var eget = type_binop2(ctx, op, eget, e2, True, WithType(eget.etype), p);
												unify(ctx, eget.etype, r_get, p);
												var Tuple(cf_set, tf_set, r_set, ekey, eget) = Codegen.AbstractCast.find_array_access(ctx, a, tl, ekey, Some(eget), p);
												var eget = switch (eget) {
												case None: assert False;
												case Some(e): e;
											};
												var et = type_module_type(ctx, TClassDecl(c), None, p);
												switch ((new Tuple(cf_set.cf_expr, cf_get.cf_expr))) {
												case (None, None): var ea = mk(TArray(ebase, ekey), r_get, p);
												mk(TBinop(OpAssignOp(op), ea, type_expr(ctx, e2, WithType(r_get))), r_set, p);
												case (Some(_), Some(_)): var ef_set = mk(TField(et, FStatic(c, cf_set)), tf_set, p);
												switch (l([])) {
												case None: make_call(ctx, ef_set, ::(ebase, ::(ekey, ::(eget, []))), r_set, p);
												case Some(e): mk(TBlock(::(e, ::(make_call(ctx, ef_set, ::(ebase, ::(ekey, ::(eget, []))), r_set, p), []))), r_set, p);
											};
												case _: error("Invalid array access getter / setter combination", p);
											};
												case AKInline(_) | AKMacro(_): assert False;
											};
												case _: var wt = switch (with_type) {
												case WithType(t) | WithTypeResume(t): switch (follow(t)) {
												case TAbstract(a, _): switch (List.filter(function (o, _): ||(=(o, OpAssignOp(op)), ==(o, op)), a.a_ops)) {
												case ::(_, []): with_type;
												case _: Value;
											};
												case _: Value;
											};
												case _: Value;
											};
												var e1 = type_expr(ctx, e1, wt);
												type_binop2(ctx, op, e1, e2, is_assign_op, wt, p);
											};
											};

												public static function type_binop2(ctx, op, e1texpr, e2Ast.expr, is_assign_op, wt, p) return {
												var e2 = type_expr(ctx, e2, if (||(==(op, OpEq), ==(op, OpNotEq))) {
												WithType(e1.etype);
											} else {
												wt;
											});
												var tint = ctx.t.tint;
												var tfloat = ctx.t.tfloat;
												var tstring = ctx.t.tstring;
												function to_string(e) return {
												function loop(t) return {
												switch (classify(t)) {
												case KAbstract({ a_impl = Some(c) }, _) if(PMap.mem("toString", c.cl_statics)): call_to_string(ctx, c, e);
												case KInt | KFloat | KString: e;
												case KUnk | KDyn | KParam(_) | KOther: var std = type_type(ctx, (new Tuple([], "Std")), e.epos);
												var acc = acc_get(ctx, type_field(ctx, std, "string", e.epos, MCall), e.epos);
												ignore(follow(acc.etype));
												var acc = switch (acc.eexpr) {
												case TField(e, FClosure(Some(c, tl), f)): { (acc) with eexpr = TField(e, FInstance(c, tl, f)) };
												case _: acc;
											};
												make_call(ctx, acc, ::(e, []), ctx.t.tstring, e.epos);
												case KAbstract(a, tl): loop(Abstract.get_underlying_type(a, tl));
											};
											};
												loop(e.etype);
											};
												function mk_op(e1, e2, t) return {
												if (&&(=(op, OpAdd), =(classify(t), KString))) {
												var e1 = to_string(e1);
												var e2 = to_string(e2);
												mk(TBinop(op, e1, e2), t, p);
											} else {
												mk(TBinop(op, e1, e2), t, p);
											};
											};
												function make(e1, e2) return {
												switch (op) {
												case OpAdd: mk_op(e1, e2, switch ((new Tuple(classify(e1.etype), classify(e2.etype)))) {
												case (KInt, KInt): tint;
												case (KFloat, KInt) | (KInt, KFloat) | (KFloat, KFloat): tfloat;
												case (KUnk, KInt): if (unify_int(ctx, e1, KUnk)) {
												tint;
											} else {
												tfloat;
											};
												case (KUnk, KFloat) | (KUnk, KString): unify(ctx, e1.etype, e2.etype, e1.epos);
												e1.etype;
												case (KInt, KUnk): if (unify_int(ctx, e2, KUnk)) {
												tint;
											} else {
												tfloat;
											};
												case (KFloat, KUnk) | (KString, KUnk): unify(ctx, e2.etype, e1.etype, e2.epos);
												e2.etype;
												case (_, KString) | (KString, _): tstring;
												case (_, KDyn): e2.etype;
												case (KDyn, _): e1.etype;
												case (KUnk, KUnk): var ok1 = unify_int(ctx, e1, KUnk);
												var ok2 = unify_int(ctx, e2, KUnk);
												if (&&(ok1, ok2)) {
												tint;
											} else {
												tfloat;
											};
												case (KParam(t1), KParam(t2)) if(Type.type_iseq(t1, t2)): t1;
												case (KParam(t), KInt) | (KInt, KParam(t)): t;
												case (KParam(_), KFloat) | (KFloat, KParam(_)) | (KParam(_), KParam(_)): tfloat;
												case (KParam(t), KUnk): unify(ctx, e2.etype, tfloat, e2.epos);
												tfloat;
												case (KUnk, KParam(t)): unify(ctx, e1.etype, tfloat, e1.epos);
												tfloat;
												case (KAbstract(_), KFloat): unify(ctx, e1.etype, tfloat, e1.epos);
												tfloat;
												case (KFloat, KAbstract(_)): unify(ctx, e2.etype, tfloat, e2.epos);
												tfloat;
												case (KAbstract(_), KInt): unify(ctx, e1.etype, ctx.t.tint, e1.epos);
												ctx.t.tint;
												case (KInt, KAbstract(_)): unify(ctx, e2.etype, ctx.t.tint, e2.epos);
												ctx.t.tint;
												case (KAbstract(_), _) | (_, KAbstract(_)) | (KParam(_), _) | (_, KParam(_)) | (KOther, _) | (_, KOther): var pr = print_context([]);
												error(^("Cannot add ", ^(s_type(pr, e1.etype), ^(" and ", s_type(pr, e2.etype)))), p);
											});
												case OpAnd | OpOr | OpXor | OpShl | OpShr | OpUShr: var i = tint;
												unify(ctx, e1.etype, i, e1.epos);
												unify(ctx, e2.etype, i, e2.epos);
												mk_op(e1, e2, i);
												case OpMod | OpMult | OpDiv | OpSub: var result = ref(if (=(op, OpDiv)) {
												tfloat;
											} else {
												tint;
											});
												switch ((new Tuple(classify(e1.etype), classify(e2.etype)))) {
												case (KFloat, KFloat): result.val = tfloat;
												case (KParam(t1), KParam(t2)) if(Type.type_iseq(t1, t2)): if (<>(op, OpDiv)) {
												result.val = t1;
											} else {
												[];
											};
												case (KParam(_), KParam(_)): result.val = tfloat;
												case (KParam(t), KInt) | (KInt, KParam(t)): if (<>(op, OpDiv)) {
												result.val = t;
											} else {
												[];
											};
												case (KParam(_), KFloat) | (KFloat, KParam(_)): result.val = tfloat;
												case (KFloat, k): ignore(unify_int(ctx, e2, k));
												result.val = tfloat;
												case (k, KFloat): ignore(unify_int(ctx, e1, k));
												result.val = tfloat;
												case (k1, k2): var ok1 = unify_int(ctx, e1, k1);
												var ok2 = unify_int(ctx, e2, k2);
												if (||(!(ok1), !(ok2))) {
												result.val = tfloat;
											} else {
												[];
											};
											};
												mk_op(e1, e2, result.val);
												case OpEq | OpNotEq: var Tuple(e1, e2) = try {
												switch (follow(e2.etype)) {
												case TAbstract({ a_path = ([], Void) }, _): error("Cannot compare Void", p);
												case _: [];
											};
												(new Tuple(Codegen.AbstractCast.cast_or_unify_raise(ctx, e2.etype, e1, p), e2));
											} catch(e:Error(Unify(_))(_)) {
												(new Tuple(e1, Codegen.AbstractCast.cast_or_unify(ctx, e1.etype, e2, p)));
											};
												mk_op(e1, e2, ctx.t.tbool);
												case OpGt | OpGte | OpLt | OpLte: switch ((new Tuple(classify(e1.etype), classify(e2.etype)))) {
												case (KInt, KInt) | (KInt, KFloat) | (KFloat, KInt) | (KFloat, KFloat) | (KString, KString): [];
												case (KInt, KUnk): ignore(unify_int(ctx, e2, KUnk));
												case (KFloat, KUnk) | (KString, KUnk): unify(ctx, e2.etype, e1.etype, e2.epos);
												case (KUnk, KInt): ignore(unify_int(ctx, e1, KUnk));
												case (KUnk, KFloat) | (KUnk, KString): unify(ctx, e1.etype, e2.etype, e1.epos);
												case (KUnk, KUnk): ignore(unify_int(ctx, e1, KUnk));
												ignore(unify_int(ctx, e2, KUnk));
												case (KDyn, KInt) | (KDyn, KFloat) | (KDyn, KString): [];
												case (KInt, KDyn) | (KFloat, KDyn) | (KString, KDyn): [];
												case (KDyn, KDyn): [];
												case (KParam(_), x) | (x, KParam(_)) if(&&(<>(x, KString), <>(x, KOther))): [];
												case (KAbstract(_), _) | (_, KAbstract(_)) | (KDyn, KUnk) | (KUnk, KDyn) | (KString, KInt) | (KString, KFloat) | (KInt, KString) | (KFloat, KString) | (KParam(_), _) | (_, KParam(_)) | (KOther, _) | (_, KOther): var pr = print_context([]);
												error(^("Cannot compare ", ^(s_type(pr, e1.etype), ^(" and ", s_type(pr, e2.etype)))), p);
											};
												mk_op(e1, e2, ctx.t.tbool);
												case OpBoolAnd | OpBoolOr: var b = ctx.t.tbool;
												unify(ctx, e1.etype, b, p);
												unify(ctx, e2.etype, b, p);
												mk_op(e1, e2, b);
												case OpInterval: var t = Typeload.load_core_type(ctx, "IntIterator");
												unify(ctx, e1.etype, tint, e1.epos);
												unify(ctx, e2.etype, tint, e2.epos);
												mk(TNew(switch (t) {
												case TInst(c, []): c;
												case _: assert False;
											}, [], ::(e1, ::(e2, []))), t, p);
												case OpArrow: error("Unexpected => ", p);
												case OpAssign | OpAssignOp(_): assert False;
											};
											};
												function find_overload(a, c, tl, left) return {
												var map = apply_params(a.a_params, tl);
												function make(op_cf, cf, e1, e2, tret) return {
												if (=(cf.cf_expr, None)) {
												if (!(Meta.has(Meta.NoExpr, cf.cf_meta))) {
												display_error(ctx, "Recursive operator method", p);
											} else {
												[];
											};
												if (!(Meta.has(Meta.CoreType, a.a_meta))) {
												var e' = make({ (e1) with etype = Abstract.follow_with_abstracts(e1.etype) }, { (e1) with etype = Abstract.follow_with_abstracts(e2.etype) });
												var t_expected = e'.etype;
												try {
												unify_raise(ctx, tret, t_expected, p);
											} catch(e:Error(Unify(_))(_)) {
												switch (follow(tret)) {
												case TAbstract(a, tl) if(type_iseq(Abstract.get_underlying_type(a, tl), t_expected)): [];
												case _: var st = s_type(print_context([]));
												error(Printf.sprintf("The result of this operation [ % s] is not compatible with declared return type %
													s", st(t_expected), st(tret)), p);
												};
												};
												} else {
													[];
												};
													var e = Codegen.binop(op, e1, e2, tret, p);
													mk_cast(e, tret, p);
												} else {
													var e = make_static_call(ctx, c, cf, map, ::(e1, ::(e2, [])), tret, p);
													e;
												};
												};
													var is_eq_op = switch (op) {
													case OpEq | OpNotEq: True;
													case _: False;
												};
													if (is_eq_op) {
													switch ((new Tuple(follow(e1.etype), follow(e2.etype)))) {
													case (TMono(_), _) | (_, TMono(_)): Type.unify(e1.etype, e2.etype);
													case _: [];
												};
												} else {
													[];
												};
													function loop(ol) return {
													switch (ol) {
													case ::((op_cf, cf), ol) if(&&(<>(op_cf, op), ||(!(is_assign_op), <>(op_cf, OpAssignOp(op))))): loop(ol);
													case ::((op_cf, cf), ol): var is_impl = Meta.has(Meta.Impl, cf.cf_meta);
													switch (follow(cf.cf_type)) {
													case TFun(::((_, _, t1), ::((_, _, t2), [])), tret): 	function check(e1, e2, swapped) return {
													function map_arguments([]) return {
													var monos = List.map(function _: mk_mono([]), cf.cf_params);
													function map(t) return {
													map(apply_params(cf.cf_params, monos, t));
												};
													var t1 = map(t1);
													var t2 = map(t2);
													var tret = map(tret);
													(new Tuple(monos, t1, t2, tret));
												};
													var Tuple(monos, t1, t2, tret) = map_arguments([]);
													function make(e1, e2) return {
													make(op_cf, cf, e1, e2, tret);
												};
													var t1 = if (is_impl) {
													Abstract.follow_with_abstracts(t1);
												} else {
													t1;
												};
													var Tuple(e1, e2) = if (||(left, &&(!(left), swapped))) {
													Type.type_eq(EqStrict, if (is_impl) {
													Abstract.follow_with_abstracts(e1.etype);
												} else {
													e1.etype;
												}, t1);
													(new Tuple(e1, Codegen.AbstractCast.cast_or_unify_raise(ctx, t2, e2, p)));
												} else {
													Type.type_eq(EqStrict, e2.etype, t2);
													(new Tuple(Codegen.AbstractCast.cast_or_unify_raise(ctx, t1, e1, p), e2));
												};
													check_constraints(ctx, "", cf.cf_params, monos, apply_params(a.a_params, tl), False, cf.cf_pos);
													function check_null(e, t) return {
													if (is_eq_op) {
													switch (e.eexpr) {
													case TConst(TNull) if(!(is_explicit_null(t))): raise(Unify_error([]));
													case _: [];
												};
												} else {
													[];
												};
												};
													if (is_eq_op) {
													check_null(e2, t2);
													check_null(e1, t1);
												} else {
													[];
												};
													var e = if (!(swapped)) {
													make(e1, e2);
												} else {
													if (&&(!(Optimizer.has_side_effect(e1)), !(Optimizer.has_side_effect(e2)))) {
													make(e1, e2);
												} else {
													var Tuple(v1, v2) = (new Tuple(gen_local(ctx, t1), gen_local(ctx, t2)));
													var Tuple(ev1, ev2) = (new Tuple(mk(TVar(v1, Some(e1)), ctx.t.tvoid, p), mk(TVar(v2, Some(e2)), ctx.t.tvoid, p)));
													var Tuple(eloc1, eloc2) = (new Tuple(mk(TLocal(v1), v1.v_type, p), mk(TLocal(v2), v2.v_type, p)));
													var e = make(eloc1, eloc2);
													var e = mk(TBlock(::(ev2, ::(ev1, ::(e, [])))), e.etype, e.epos);
													e;
												};
												};
													if (&&(is_assign_op, =(op_cf, op))) {
													mk(TMeta((new Tuple(Meta.RequiresAssign, [], p)), e), e.etype, e.epos);
												} else {
													e;
												};
												};
													try {
													check(e1, e2, False);
												} catch(e:Error(Unify(_), _) | Unify_error(_)) {
													try {
													if (!(Meta.has(Meta.Commutative, cf.cf_meta))) {
													raise(Not_found);
												} else {
													[];
												};
													check(e2, e1, True);
												} catch(e:Not_found | Error(Unify(_), _) | Unify_error(_)) {
													loop(ol);
												};
												};
													case _: assert False;
												};
													case []: raise(Not_found);
												};
												};
													loop(if (left) {
													a.a_ops;
												} else {
													List.filter(function (_, cf): !(Meta.has(Meta.Impl, cf.cf_meta)), a.a_ops);
												});
												};
													try {
													switch (follow(e1.etype)) {
													case TAbstract({ a_impl = Some(c) } = a, tl): find_overload(a, c, tl, True);
													case _: raise(Not_found);
												};
												} catch(e:Not_found) {
													try {
													switch (follow(e2.etype)) {
													case TAbstract({ a_impl = Some(c) } = a, tl): find_overload(a, c, tl, False);
													case _: raise(Not_found);
												};
												} catch(e:Not_found) {
													make(e1, e2);
												};
												};
												};

													public static function type_unop(ctx, op, flag, e, p) return {
													var set = ||(=(op, Increment), =(op, Decrement));
													var acc = type_access(ctx, fst(e), snd(e), if (set) {
													MSet;
												} else {
													MGet;
												});
													function access(e) return {
													function make(e) return {
													var t = switch (op) {
													case Not: unify(ctx, e.etype, ctx.t.tbool, e.epos);
													ctx.t.tbool;
													case Increment | Decrement | Neg | NegBits: if (set) {
													check_assign(ctx, e);
												} else {
													[];
												};
													switch (classify(e.etype)) {
													case KFloat: ctx.t.tfloat;
													case KParam(t): unify(ctx, e.etype, ctx.t.tfloat, e.epos);
													t;
													case k: if (unify_int(ctx, e, k)) {
													ctx.t.tint;
												} else {
													ctx.t.tfloat;
												};
												};
												};
													mk(TUnop(op, flag, e), t, p);
												};
													try {
													switch (follow(e.etype)) {
													case TAbstract({ a_impl = Some(c) } = a, pl): 	function loop(opl) return {
													switch (opl) {
													case []: raise(Not_found);
													case ::((op2, flag2, cf), opl) if(&&(==(op, op2), ==(flag, flag2))): var m = mk_mono([]);
													var tcf = apply_params(a.a_params, pl, monomorphs(cf.cf_params, cf.cf_type));
													if (Meta.has(Meta.Impl, cf.cf_meta)) {
													if (type_iseq(tfun(::(apply_params(a.a_params, pl, a.a_this), []), m), tcf)) {
													(new Tuple(cf, tcf, m));
												} else {
													loop(opl);
												};
												} else {
													if (type_iseq(tfun(::(e.etype, []), m), tcf)) {
													(new Tuple(cf, tcf, m));
												} else {
													loop(opl);
												};
												};
													case ::(_, opl): loop(opl);
												};
												};
													var Tuple(cf, t, r) = try {
													loop(a.a_unops);
												} catch(e:Not_found) {
													raise(Not_found);
												};
													switch (cf.cf_expr) {
													case None: var e = { (e) with etype = apply_params(a.a_params, pl, a.a_this) };
													var e = mk(TUnop(op, flag, e), r, p);
													e;
													case Some(_): var et = type_module_type(ctx, TClassDecl(c), None, p);
													var ef = mk(TField(et, FStatic(c, cf)), t, p);
													make_call(ctx, ef, ::(e, []), r, p);
												};
													case _: raise(Not_found);
												};
												} catch(e:Not_found) {
													make(e);
												};
												};
													function loop(acc) return {
													switch (acc) {
													case AKExpr(e): access(e);
													case AKInline(_) | AKUsing(_) if(!(set)): access(acc_get(ctx, acc, p));
													case AKNo(s): error(^("The field or identifier ", ^(s, ^(" is not accessible for ", if (set) {
													"writing";
												} else {
													"reading";
												}))), p);
													case AKAccess(a, tl, c, ebase, ekey): var e = mk_array_get_call(ctx, Codegen.AbstractCast.find_array_access(ctx, a, tl, ekey, None, p), c, ebase, p);
													loop(AKExpr(e));
													case AKInline(_) | AKUsing(_) | AKMacro(_): error("This kind of operation is not supported", p);
													case AKSet(e, t, cf): var l = save_locals(ctx);
													var v = gen_local(ctx, e.etype);
													var ev = mk(TLocal(v), e.etype, p);
													var op = switch (op) {
													case Increment: OpAdd;
													case Decrement: OpSub;
													case _: assert False;
												};
													var one = (new Tuple(EConst(Int("1")), p));
													var eget = (new Tuple(EField((new Tuple(EConst(Ident(v.v_name)), p)), cf.cf_name), p));
													switch (flag) {
													case Prefix: var get = type_binop(ctx, op, eget, one, False, Value, p);
													unify(ctx, get.etype, t, p);
													l([]);
													mk(TBlock(::(mk(TVar(v, Some(e)), ctx.t.tvoid, p), ::(make_call(ctx, mk(TField(ev, quick_field_dynamic(ev.etype, ^("set_", cf.cf_name))), tfun(::(t, []), t), p), ::(get, []), t, p), []))), t, p);
													case Postfix: var v2 = gen_local(ctx, t);
													var ev2 = mk(TLocal(v2), t, p);
													var get = type_expr(ctx, eget, Value);
													var plusone = type_binop(ctx, op, (new Tuple(EConst(Ident(v2.v_name)), p)), one, False, Value, p);
													unify(ctx, get.etype, t, p);
													l([]);
													mk(TBlock(::(mk(TVar(v, Some(e)), ctx.t.tvoid, p), ::(mk(TVar(v2, Some(get)), ctx.t.tvoid, p), ::(make_call(ctx, mk(TField(ev, quick_field_dynamic(ev.etype, ^("set_", cf.cf_name))), tfun(::(plusone.etype, []), t), p), ::(plusone, []), t, p), ::(ev2, []))))), t, p);
												};
												};
												};
													loop(acc);
												};

													public static function type_switch_old(ctx, e, cases, def, with_type, p) return {
													var eval = type_expr(ctx, e, Value);
													var el = ref([]);
													function type_case_code(e) return {
													var e = switch (e) {
													case Some(e): type_expr(ctx, e, with_type);
													case None: mk(TBlock([]), ctx.com.basic.tvoid, Ast.null_pos);
												};
													el.val = ::(e, el.val);
													e;
												};
													var consts = Hashtbl.create(0);
													function exprs(Tuple(el, _, e)) return {
													var el = List.map(function e: switch (type_expr(ctx, e, WithType(eval.etype))) {
													case { eexpr = TConst(c) } = e: if (Hashtbl.mem(consts, c)) {
													error("Duplicate constant in switch ", e.epos);
													} else {
														[];
													};
														Hashtbl.add(consts, c, True);
														e;
														case e: e;
													}, el);
														var locals = save_locals(ctx);
														var e = type_case_code(e);
														locals([]);
														(new Tuple(el, e));
													};
														var cases = List.map(exprs, cases);
														function def([]) return {
														switch (def) {
														case None: None;
														case Some(e): var locals = save_locals(ctx);
														var e = type_case_code(e);
														locals([]);
														Some(e);
													};
													};
														var def = def([]);
														var t = if (=(with_type, NoValue)) {
														mk_mono([]);
													} else {
														unify_min(ctx, List.rev(el.val));
													};
														mk(TSwitch(eval, cases, def), t, p);
													};

														public static function type_ident(ctx, i, p, mode) return {
														try {
														type_ident_raise(ctx, i, p, mode);
													} catch(e:Not_found) {
														try {
														if (is_lower_ident(i)) {
														raise(Not_found);
													} else {
														[];
													};
														var e = try {
														type_type(ctx, (new Tuple([], i)), p);
													} catch(e:Error(Module_not_found([])(name))(_)) {
														raise(Not_found);
													};
														AKExpr(e);
													} catch(e:Not_found) {
														if (ctx.untyped) {
														if (=(i, "__this__")) {
														AKExpr(mk(TConst(TThis), ctx.tthis, p));
													} else {
														var t = mk_mono([]);
														var v = alloc_unbound_var(i, t);
														AKExpr(mk(TLocal(v), t, p));
													};
													} else {
														if (&&(=(ctx.curfun, FunStatic), PMap.mem(i, ctx.curclass.cl_fields))) {
														error(^("Cannot access ", ^(i, " in static function")), p);
													} else {
														[];
													};
														var err = Unknown_ident(i);
														if (ctx.in_display) {
														raise(Error(err, p));
													} else {
														[];
													};
														if (<>(ctx.com.display, DMNone)) {
														display_error(ctx, error_msg(err), p);
														var t = mk_mono([]);
														AKExpr(mk(TLocal(add_local(ctx, i, t)), t, p));
													} else {
														if (List.exists(function (i2, _): =(i2, i), ctx.type_params)) {
														display_error(ctx, ^("Type parameter ", ^(i, " is only available at compilation and is not a runtime value")), p);
													} else {
														display_error(ctx, error_msg(err), p);
													};
														AKExpr(mk(TConst(TNull), t_dynamic, p));
													};
													};
													};
													};
													};

														public static function type_access(ctx, e, p, mode) return {
														switch (e) {
														case EConst(Ident(s)): type_ident(ctx, s, p, mode);
														case EField(e1, new): var e1 = type_expr(ctx, e1, Value);
														switch (e1.eexpr) {
														case TTypeExpr(TClassDecl(c)): if (=(mode, MSet)) {
														error("Cannot set constructor", p);
													} else {
														[];
													};
														if (=(mode, MCall)) {
														error(^("Cannot call constructor like this, use 'new ", ^(s_type_path(c.cl_path), "[]' instead")), p);
													} else {
														[];
													};
														var monos = List.map(function _: mk_mono([]), c.cl_params);
														var Tuple(ct, cf) = get_constructor(ctx, c, monos, p);
														var args = switch (follow(ct)) {
														case TFun(args, ret): args;
														case _: assert False;
													};
														var vl = List.map(function (n, _, t): alloc_var(n, t), args);
														function vexpr(v) return {
														mk(TLocal(v), v.v_type, p);
													};
														var el = List.map(vexpr, vl);
														var Tuple(ec, t) = switch (c.cl_kind) {
														case KAbstractImpl(a): var e = type_module_type(ctx, TClassDecl(c), None, p);
														var e = mk(TField(e, FStatic(c, cf)), ct, p);
														var t = TAbstract(a, monos);
														(new Tuple(make_call(ctx, e, el, t, p), t));
														case _: var t = TInst(c, monos);
														(new Tuple(mk(TNew(c, monos, el), t, p), t));
													};
														AKExpr(mk(TFunction({ () with tf_args = List.map(function v: (new Tuple(v, None)), vl);
														tf_type = t;
														tf_expr = mk(TReturn(Some(ec)), t, p) }), tfun(List.map(function v: v.v_type, vl), t), p));
														case _: error("Binding new is only allowed on class types", p);
														};
															case EField(_): 	function fields(?:(resume = False), path, e) return {
															var resume = ref(resume);
															var force = ref(False);
															var e = List.fold_left(function e: function (f, _, p): var e = acc_get(ctx, e(MGet), p);
															var f = type_field(resume = resume.val, ctx, e, f, p);
															force.val = resume.val;
															resume.val = False;
															f, e, path);
															if (force.val) {
															ignore(e(MCall));
														} else {
															[];
														};
															e;
														};
															function type_path(path) return {
															function loop(acc, path) return {
															switch (path) {
															case []: switch (List.rev(acc)) {
															case []: assert False;
															case ::((name, flag, p), path): try {
															fields(path, type_access(ctx, EConst(Ident(name)), p));
														} catch(e:Error(Unknown_ident(_))(p2) = e) {
															try {
															var path = ref([]);
															var Tuple(name, _, _) = List.find(function (name, flag, p): if (flag) {
															True;
														} else {
															path.val = ::(name, path.val);
															False;
														}, List.rev(acc));
															raise(Error(Module_not_found(List.rev(path.val), name), p));
														} catch(e:Not_found) {
															if (ctx.in_display) {
															raise(Parser.TypePath(List.map(function (n, _, _): n, List.rev(acc)), None, False));
														} else {
															[];
														};
															raise(e);
														};
														};
														};
															case ::((_, False, _) = x, path): loop(::(x, acc), path);
															case ::((name, True, p) = x, path): var pack = List.rev_map(function (x, _, _): x, acc);
															function def([]) return {
															try {
															var e = type_type(ctx, (new Tuple(pack, name)), p);
															fields(path, function _: AKExpr(e));
														} catch(e:Error(Module_not_found(m))(_)) {
															loop(@(List.rev(path), ::(x, acc)), []);
														};
														};
															switch (path) {
															case ::((sname, True, p), path): 	function get_static(resume, t) return {
															fields(resume = , ::((new Tuple(sname, True, p)), path), function _: AKExpr(type_module_type(ctx, t, None, p)));
														};
															function check_module(m, v) return {
															try {
															var md = Typeload.load_module(ctx, m, p);
															try {
															var t = List.find(function t: &&(!(t_infos(t).mt_private), =(t_path(t), (new Tuple(fst(m), sname)))), md.m_types);
															Some(fields(path, function _: AKExpr(type_module_type(ctx, t, None, p))));
														} catch(e:Not_found) {
															try {
															if (=(fst(m), [])) {
															raise(Not_found);
														} else {
															[];
														};
															var t = List.find(function t: &&(!(t_infos(t).mt_private), =(t_path(t), m)), md.m_types);
															Some(get_static(False, t));
														} catch(e:Not_found) {
															None;
														};
														};
														} catch(e:Error(Module_not_found(m2))(_)) {
															None;
														};
														};
															function loop(pack) return {
															switch (check_module((new Tuple(pack, name)), sname)) {
															case Some(r): r;
															case None: switch (List.rev(pack)) {
															case []: def([]);
															case ::(_, l): loop(List.rev(l));
														};
														};
														};
															switch (pack) {
															case []: try {
															var t = List.find(function t: =(snd(t_infos(t).mt_path), name), @(ctx.m.curmod.m_types, ctx.m.module_types));
															get_static(True, t);
														} catch(e:Not_found) {
															loop(fst(ctx.m.curmod.m_path));
														};
															case _: switch (check_module((new Tuple(pack, name)), sname)) {
															case Some(r): r;
															case None: def([]);
														};
														};
															case _: def([]);
														};
														};
														};
															switch (path) {
															case []: assert False;
															case ::((name, _, p), pnext): try {
															fields(pnext, function _: type_ident_raise(ctx, name, p, MGet));
														} catch(e:Not_found) {
															loop([], path);
														};
														};
														};
															function loop(acc, e) return {
															var p = pos(e);
															switch (fst(e)) {
															case EField(e, s): loop(::((new Tuple(s, !(is_lower_ident(s)), p)), acc), e);
															case EConst(Ident(i)): type_path(::((new Tuple(i, !(is_lower_ident(i)), p)), acc));
															case _: fields(acc, type_access(ctx, fst(e), snd(e)));
														};
														};
															loop([], (new Tuple(e, p)), mode);
															case EArray(e1, e2): var e1 = type_expr(ctx, e1, Value);
															var e2 = type_expr(ctx, e2, Value);
															var has_abstract_array_access = ref(False);
															try {
															switch (follow(e1.etype)) {
															case TAbstract({ a_impl = Some(c) } = a, pl) if(<>(a.a_array, [])): switch (mode) {
															case MSet: AKAccess(a, pl, c, e1, e2);
															case _: has_abstract_array_access.val = True;
															var e = mk_array_get_call(ctx, Codegen.AbstractCast.find_array_access(ctx, a, pl, e2, None, p), c, e1, p);
															AKExpr(e);
														};
															case _: raise(Not_found);
														};
														} catch(e:Not_found) {
															unify(ctx, e2.etype, ctx.t.tint, e2.epos);
															function loop(et) return {
															switch (follow(et)) {
															case TInst({ cl_array_access = Some(t); cl_params = pl }, tl): apply_params(pl, tl, t);
															case TInst({ cl_super = Some(c, stl); cl_params = pl }, tl): apply_params(pl, tl, loop(TInst(c, stl)));
															case TInst({ cl_path = ([], ArrayAccess) }, ::(t, [])): t;
															case TInst({ cl_path = ([], Array) }, ::(t, [])) if(==(t, t_dynamic)): t_dynamic;
															case TAbstract(a, tl) if(Meta.has(Meta.ArrayAccess, a.a_meta)): loop(apply_params(a.a_params, tl, a.a_this));
															case _: var pt = mk_mono([]);
															var t = ctx.t.tarray(pt);
															try {
															unify_raise(ctx, et, t, p);
														} catch(e:Error(Unify(_))(_)) {
															if (!(ctx.untyped)) {
															if (has_abstract_array_access.val) {
															error(^("No @: arrayAccess function accepts an argument of ", s_type(print_context([]), e2.etype)), e1.epos);
														} else {
															error(^("Array access is not allowed on ", s_type(print_context([]), e1.etype)), e1.epos);
														};
														} else {
															[];
														};
														};
															pt;
														};
														};
															var pt = loop(e1.etype);
															AKExpr(mk(TArray(e1, e2), pt, p));
														};
															case _: AKExpr(type_expr(ctx, (new Tuple(e, p)), Value));
														};
														};

															public static function type_vars(ctx, vl, p, in_block) return {
															var save = if (in_block) {
															function []: [];
														} else {
															save_locals(ctx);
														};
															var vl = List.map(function (v, t, e): try {
															var t = Typeload.load_type_opt(ctx, p, t);
															var e = switch (e) {
															case None: None;
															case Some(e): var e = type_expr(ctx, e, WithType(t));
															var e = Codegen.AbstractCast.cast_or_unify(ctx, t, e, p);
															Some(e);
														};
															if (&&(=(v0, '$'), =(ctx.com.display, DMNone))) {
															error("Variables names starting with a dollar are not allowed", p);
														} else {
															[];
														};
															(new Tuple(add_local(ctx, v, t), e));
														} catch(e:Error(e)(p)) {
															display_error(ctx, error_msg(e), p);
															(new Tuple(add_local(ctx, v, t_dynamic), None));
														}, vl);
															save([]);
															switch (vl) {
															case ::((v, eo), []): mk(TVar(v, eo), ctx.t.tvoid, p);
															case _: var e = mk(TBlock(List.map(function (v, e): mk(TVar(v, e), ctx.t.tvoid, p), vl)), ctx.t.tvoid, p);
															mk(TMeta((new Tuple(Meta.MergeBlock, [], p)), e), e.etype, e.epos);
														};
														};

															public static function with_type_error(ctx, with_type, msg, p) return {
															switch (with_type) {
															case WithTypeResume(_): raise(WithTypeError(::(Unify_custom(msg), []), p));
															case _: display_error(ctx, msg, p);
														};
														};

															public static function format_string(ctx, s, p) return {
															var e = ref(None);
															var pmin = ref(p.pmin);
															var min = ref(+(p.pmin, 1));
															function add(enext, len) return {
															var p = { (p) with pmin = min.val;
															pmax = +(min.val, len) };
															min.val = +(min.val, len);
															switch (e.val) {
															case None: e.val = Some(enext, p);
															case Some(prev): e.val = Some(EBinop(OpAdd, prev, (new Tuple(enext, p))), punion(pos(prev), p));
														};
														};
															function add_sub(start, pos) return {
															var len = -(pos, start);
															if (||(>(len, 0), =(e.val, None))) {
															add(EConst(String(String.sub(s, start, len))), len);
														} else {
															[];
														};
														};
															var warn_escape = Common.defined(ctx.com, Define.FormatWarning);
															function warn(pos, len) return {
															ctx.com.warning("This string is formated", { (p) with pmin = +(+(pmin.val, 1), pos);
															pmax = +(+(+(pmin.val, 1), pos), len) });
														};
															var len = String.length(s);
															function parse(start, pos) return {
															if (=(pos, len)) {
															add_sub(start, pos);
														} else {
															var c = String.unsafe_get(s, pos);
															var pos = +(pos, 1);
															if (=(c, '\'')) {
															incr(pmin);
															incr(min);
														} else {
															[];
														};
															if (||(<>(c, '$'), =(pos, len))) {
															parse(start, pos);
														} else {
															switch (String.unsafe_get(s, pos)) {
															case '$': if (warn_escape) {
															warn(pos, 1);
														} else {
															[];
														};
															add_sub(start, pos);
															parse(+(pos, 1), +(pos, 1));
															case '{': parse_group(start, pos, '{', '}', "brace");
															case 'a' .. 'z' | 'A' .. 'Z' | '_': add_sub(start, -(pos, 1));
															incr(min);
															function loop(i) return {
															if (=(i, len)) {
															i;
														} else {
															var c = String.unsafe_get(s, i);
															switch (c) {
															case 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_': loop(+(i, 1));
															case _: i;
														};
														};
														};
															var iend = loop(+(pos, 1));
															var len = -(iend, pos);
															if (warn_escape) {
															warn(pos, len);
														} else {
															[];
														};
															add(EConst(Ident(String.sub(s, pos, len))), len);
															parse(+(pos, len), +(pos, len));
															case _: parse(start, pos);
														};
														};
														};
														};
															function parse_group(start, pos, gopen, gclose, gname) return {
															add_sub(start, -(pos, 1));
															function loop(groups, i) return {
															if (=(i, len)) {
															switch (groups) {
															case []: assert False;
															case ::(g, _): error(^("Unclosed ", gname), { (p) with pmin = +(+(pmin.val, g), 1);
															pmax = +(+(pmin.val, g), 2) });
														};
														} else {
															var c = String.unsafe_get(s, i);
															if (=(c, gopen)) {
															loop(::(i, groups), +(i, 1));
														} else {
															if (=(c, gclose)) {
															var groups = List.tl(groups);
															if (=(groups, [])) {
															i;
														} else {
															loop(groups, +(i, 1));
														};
														} else {
															loop(groups, +(i, 1));
														};
														};
														};
														};
															var send = loop(::(pos, []), +(pos, 1));
															var slen = -(-(send, pos), 1);
															var scode = String.sub(s, +(pos, 1), slen);
															if (warn_escape) {
															warn(+(pos, 1), slen);
														} else {
															[];
														};
															min.val = +(min.val, 2);
															if (>(slen, 0)) {
															add(fst(parse_expr_string(ctx, scode, { (p) with pmin = +(+(pmin.val, pos), 2);
															pmax = +(+(pmin.val, send), 1) }, True)), slen);
														} else {
															[];
														};
															min.val = +(min.val, 1);
															parse(+(send, 1), +(send, 1));
														};
															parse(0, 0);
															switch (e.val) {
															case None: assert False;
															case Some(e): e;
														};
														};

															public static function type_block(ctx, el, with_type, p) return {
															function merge(e) return {
															switch (e.eexpr) {
															case TMeta((Meta.MergeBlock, _, _), { eexpr = TBlock(el) }): el;
															case _: ::(e, []);
														};
														};
															function loop(match) return switch (match) {
															case []: [];
															case ::((EVars(vl), p), l): var e = type_vars(ctx, vl, p, True);
															@(merge(e), loop(l));
															case ::(e, []): try {
															merge(type_expr(ctx, e, with_type));
														} catch(e:Error(e)(p)) {
															display_error(ctx, error_msg(e), p);
															[];
														};
															case ::(e, l): try {
															var e = type_expr(ctx, e, NoValue);
															@(merge(e), loop(l));
														} catch(e:Error(e)(p)) {
															display_error(ctx, error_msg(e), p);
															loop(l);
														};
														};
															var l = loop(el);
															function loop(match) return switch (match) {
															case []: ctx.t.tvoid;
															case ::(e, []): e.etype;
															case ::(_, l): loop(l);
														};
															mk(TBlock(l), loop(l), p);
														};

															public static function type_expr(ctx, Tuple(e, p), with_typewith_type) return {
															switch (e) {
															case EField((EConst(String(s)), p), code): if (<>(UTF8.length(s), 1)) {
															error("String must be a single UTF8 char", p);
														} else {
															[];
														};
															mk(TConst(TInt(Int32.of_int(UChar.code(UTF8.get(s, 0))))), ctx.t.tint, p);
															case EField(_, n) if(=(n0, '$')): error("Field names starting with $ are not allowed", p);
															case EConst(Ident(s)): if (&&(=(s, "super"), <>(with_type, NoValue))) {
															error("Cannot use super as value", p);
														} else {
															[];
														};
															try {
															acc_get(ctx, type_ident_raise(imported_enums = False, ctx, s, p, MGet), p);
														} catch(e:Not_found) {
															try {
															switch (with_type) {
															case WithType(t) | WithTypeResume(t): switch (follow(t)) {
															case TEnum(e, pl): try {
															var ef = PMap.find(s, e.e_constrs);
															var monos = List.map(function _: mk_mono([]), ef.ef_params);
															mk(fast_enum_field(e, ef, p), enum_field_type(ctx, e, ef, pl, monos, p), p);
														} catch(e:Not_found) {
															if (ctx.untyped) {
															raise(Not_found);
														} else {
															[];
														};
															with_type_error(ctx, with_type, string_error(s, e.e_names, ^("Identifier '", ^(s, ^("' is not part of enum
															", s_type_path(e.e_path))))), p);
															mk(TConst(TNull), t, p);
														};
															case TAbstract(a, pl) if(has_meta(Meta.Enum, a.a_meta)): var cimpl = switch (a.a_impl) {
															case None: assert False;
															case Some(c): c;
														};
															try {
															var cf = PMap.find(s, cimpl.cl_statics);
															acc_get(ctx, type_field(ctx, mk(TTypeExpr(TClassDecl(cimpl)), TAnon({ () with a_fields = PMap.add(cf.cf_name, cf, PMap.empty);
															a_status = ref(Statics(cimpl)) }), p), s, p, MGet), p);
														} catch(e:Not_found) {
															if (ctx.untyped) {
															raise(Not_found);
														} else {
															[];
														};
															with_type_error(ctx, with_type, string_error(s, List.map(function f: f.cf_name, cimpl.cl_ordered_statics), ^("Identifier
															'", ^(s, ^("' is not part of enum ", s_type_path(a.a_path))))), p);
															mk(TConst(TNull), t, p);
														};
															case _: raise(Not_found);
														};
															case _: raise(Not_found);
														};
														} catch(e:Not_found) {
															acc_get(ctx, type_access(ctx, e, p, MGet), p);
														};
														};
															case EField(_) | EArray(_): acc_get(ctx, type_access(ctx, e, p, MGet), p);
															case EConst(Regexp(r, opt)): var str = mk(TConst(TString(r)), ctx.t.tstring, p);
															var opt = mk(TConst(TString(opt)), ctx.t.tstring, p);
															var t = Typeload.load_core_type(ctx, "EReg");
															mk(TNew(switch (t) {
															case TInst(c, []): c;
															case _: assert False;
														}, [], ::(str, ::(opt, []))), t, p);
															case EConst(String(s)) if(Lexer.is_fmt_string(p)): type_expr(ctx, format_string(ctx, s, p), with_type);
															case EConst(c): Codegen.type_constant(ctx.com, c, p);
															case EBinop(op, e1, e2): type_binop(ctx, op, e1, e2, False, with_type, p);
															case EBlock([]) if(<>(with_type, NoValue)): type_expr(ctx, (new Tuple(EObjectDecl([]), p)), with_type);
															case EBlock(l): var locals = save_locals(ctx);
															var e = type_block(ctx, l, with_type, p);
															locals([]);
															e;
															case EParenthesis(e): var e = type_expr(ctx, e, with_type);
															mk(TParenthesis(e), e.etype, p);
															case EObjectDecl(fl): var dynamic_parameter = ref(None);
															var a = switch (with_type) {
															case WithType(t) | WithTypeResume(t): switch (follow(t)) {
															case TAnon(a) if(!(PMap.is_empty(a.a_fields))): Some(a);
															case TDynamic(t) if(!=(follow(t), t_dynamic)): dynamic_parameter.val = Some(t);
															Some({ () with a_status = ref(Closed);
															a_fields = PMap.empty });
															case _: None;
														};
															case _: None;
														};
															function wrap_quoted_meta(e) return {
															mk(TMeta((new Tuple(Meta.QuotedField, [], e.epos)), e), e.etype, e.epos);
														};
															switch (a) {
															case None: 	function loop(Tuple(l, acc), Tuple(f, e)) return {
															var Tuple(f, is_quoted, is_valid) = Parser.unquote_ident(f);
															if (PMap.mem(f, acc)) {
															error(^("Duplicate field in object declaration : ", f), p);
															} else {
																[];
															};
																var e = type_expr(ctx, e, Value);
																switch (follow(e.etype)) {
																case TAbstract({ a_path = ([], Void) }, _): error("Fields of type Void are not allowed in structures", e.epos);
																case _: [];
															};
																var cf = mk_field(f, e.etype, e.epos);
																var e = if (is_quoted) {
																wrap_quoted_meta(e);
															} else {
																e;
															};
																(new Tuple(::((new Tuple(f, e)), l), if (is_valid) {
																if (&&(>(String.length(f), 0), =(f0, '$'))) {
																error("Field names starting with a dollar are not allowed", p);
															} else {
																[];
															};
																PMap.add(f, cf, acc);
															} else {
																acc;
															}));
															};
																var Tuple(fields, types) = List.fold_left(loop, (new Tuple([], PMap.empty)), fl);
																var x = ref(Const);
																ctx.opened = ::(x, ctx.opened);
																mk(TObjectDecl(List.rev(fields)), TAnon({ () with a_fields = types;
																a_status = x }), p);
																case Some(a): var fields = ref(PMap.empty);
																var extra_fields = ref([]);
																var fl = List.map(function (n, e): var Tuple(n, is_quoted, is_valid) = Parser.unquote_ident(n);
																if (PMap.mem(n, fields.val)) {
																error(^("Duplicate field in object declaration : ", n), p);
															} else {
																[];
															};
																var e = try {
																var t = switch (dynamic_parameter.val) {
																case Some(t): t;
																case None: PMap.find(n, a.a_fields).cf_type;
															};
																var e = type_expr(ctx, e, switch (with_type) {
																case WithTypeResume(_): WithTypeResume(t);
																case _: WithType(t);
															});
																var e = Codegen.AbstractCast.cast_or_unify(ctx, t, e, p);
																try {
																type_eq(EqStrict, e.etype, t);
																e;
															} catch(e:Unify_error(_)) {
																mk(TCast(e, None), t, e.epos);
															};
															} catch(e:Not_found) {
																if (is_valid) {
																extra_fields.val = ::(n, extra_fields.val);
															} else {
																[];
															};
																type_expr(ctx, e, Value);
															};
																if (is_valid) {
																if (&&(>(String.length(n), 0), =(n0, '$'))) {
																error("Field names starting with a dollar are not allowed", p);
															} else {
																[];
															};
																var cf = mk_field(n, e.etype, e.epos);
																fields.val = PMap.add(n, cf, fields.val);
															} else {
																[];
															};
																var e = if (is_quoted) {
																wrap_quoted_meta(e);
															} else {
																e;
															};
																(new Tuple(n, e)), fl);
																var t = TAnon({ () with a_fields = fields.val;
																a_status = ref(Const) });
																if (!(ctx.untyped)) {
																function unify_error(l, p) return {
																switch (with_type) {
																case WithTypeResume(_): raise(WithTypeError(l, p));
																case _: raise(Error(Unify(l), p));
															};
															};
																switch (PMap.foldi(function n: function cf: function acc: if (&&(!(Meta.has(Meta.Optional, cf.cf_meta)), !(PMap.mem(n, fields.val)))) {
																::(n, acc);
															} else {
																acc;
															}, a.a_fields, [])) {
																case []: [];
																case ::(n, []): unify_error(::(Unify_custom(^("Object requires field ", n)), []), p);
																case nl: unify_error(::(Unify_custom(^("Object requires fields: ", String.concat(", ", nl))), []), p);
															};
																switch (extra_fields.val) {
																case []: [];
																case _: unify_error(List.map(function n: has_extra_field(t, n), extra_fields.val), p);
															};
															} else {
																[];
															};
																if (<>(a.a_status.val, Const)) {
																a.a_status.val = Closed;
															} else {
																[];
															};
																mk(TObjectDecl(fl), t, p);
															};
																case EArrayDecl(::((EFor(_), _) | (EWhile(_), _) = e, [])): var v = gen_local(ctx, mk_mono([]));
																var et = ref((new Tuple(EConst(Ident("null")), p)));
																function map_compr(Tuple(e, p)) return {
																switch (e) {
																case EFor(it, e2): (new Tuple(EFor(it, map_compr(e2)), p));
																case EWhile(cond, e2, flag): (new Tuple(EWhile(cond, map_compr(e2), flag), p));
																case EIf(cond, e2, None): (new Tuple(EIf(cond, map_compr(e2), None), p));
																case EBlock(::(e, [])): (new Tuple(EBlock(::(map_compr(e), [])), p));
																case EParenthesis(e2): (new Tuple(EParenthesis(map_compr(e2)), p));
																case EBinop(OpArrow, a, b): et.val = (new Tuple(ENew({ () with tpackage = [];
																tname = "Map";
																tparams = [];
																tsub = None }, []), p));
																(new Tuple(ECall((new Tuple(EField((new Tuple(EConst(Ident(v.v_name)), p)), "set"), p)), ::(a, ::(b, []))), p));
																case _: et.val = (new Tuple(EArrayDecl([]), p));
																(new Tuple(ECall((new Tuple(EField((new Tuple(EConst(Ident(v.v_name)), p)), "push"), p)), ::((new Tuple(e, p)), [])), p));
															};
															};
																var e = map_compr(e);
																var ea = type_expr(ctx, et.val, with_type);
																unify(ctx, v.v_type, ea.etype, p);
																var efor = type_expr(ctx, e, NoValue);
																mk(TBlock(::(mk(TVar(v, Some(ea)), ctx.t.tvoid, p), ::(efor, ::(mk(TLocal(v), v.v_type, p), [])))), v.v_type, p);
																case EArrayDecl(::((EBinop(OpArrow, _, _), _) = e1, el)): var Tuple(Tuple(tkey, tval, has_type), resume) = 	function get_map_params(t) return {
																switch (follow(t)) {
																case TAbstract({ a_path = ([], Map) }, ::(tk, ::(tv, []))): (new Tuple(tk, tv, True));
																case TInst({ cl_path = (::(haxe, ::(ds, [])), IntMap) }, ::(tv, [])): (new Tuple(ctx.t.tint, tv, True));
																case TInst({ cl_path = (::(haxe, ::(ds, [])), StringMap) }, ::(tv, [])): (new Tuple(ctx.t.tstring, tv, True));
																case TInst({ cl_path = (::(haxe, ::(ds, [])), ObjectMap | EnumValueMap) }, ::(tk, ::(tv, []))): (new Tuple(tk, tv, True));
																case _: (new Tuple(mk_mono([]), mk_mono([]), False));
															};
															};
																switch (with_type) {
																case WithType(t): (new Tuple(get_map_params(t), False));
																case WithTypeResume(t): (new Tuple(get_map_params(t), True));
																case _: (new Tuple((new Tuple(mk_mono([]), mk_mono([]), False)), False));
															};
																var keys = Hashtbl.create(0);
																function unify_with_resume(ctx, e, t, p) return {
																if (resume) {
																try {
																Codegen.AbstractCast.cast_or_unify_raise(ctx, t, e, p);
															} catch(e:Error(Unify(l))(p)) {
																raise(WithTypeError(l, p));
															};
															} else {
																Codegen.AbstractCast.cast_or_unify(ctx, t, e, p);
															};
															};
																function check_key(e_key) return {
																try {
																var p = Hashtbl.find(keys, e_key.eexpr);
																display_error(ctx, "Duplicate key", e_key.epos);
																error("Previously defined here", p);
															} catch(e:Not_found) {
																Hashtbl.add(keys, e_key.eexpr, e_key.epos);
															};
															};
																var el = ::(e1, el);
																var el_kv = List.map(function e: switch (fst(e)) {
																case EBinop(OpArrow, e1, e2): (new Tuple(e1, e2));
																case _: error("Expected a => b", pos(e));
															}, el);
																var Tuple(el_k, el_v, tkey, tval) = if (has_type) {
																var Tuple(el_k, el_v) = List.fold_left(function (el_k, el_v): function (e1, e2): var e1 = type_expr(ctx, e1, WithType(tkey));
																check_key(e1);
																var e1 = unify_with_resume(ctx, e1, tkey, e1.epos);
																var e2 = type_expr(ctx, e2, WithType(tval));
																var e2 = unify_with_resume(ctx, e2, tval, e2.epos);
																(new Tuple(::(e1, el_k), ::(e2, el_v))), (new Tuple([], [])), el_kv);
																(new Tuple(el_k, el_v, tkey, tval));
															} else {
																var Tuple(el_k, el_v) = List.fold_left(function (el_k, el_v): function (e1, e2): var e1 = type_expr(ctx, e1, Value);
																check_key(e1);
																var e2 = type_expr(ctx, e2, Value);
																(new Tuple(::(e1, el_k), ::(e2, el_v))), (new Tuple([], [])), el_kv);
																function unify_min_resume(el) return {
																try {
																unify_min_raise(ctx, el);
															} catch(e:Error(Unify(l))(p)) {
																raise(WithTypeError(l, p));
															};
															};
																var tkey = unify_min_resume(el_k);
																var tval = unify_min_resume(el_v);
																(new Tuple(el_k, el_v, tkey, tval));
															};
																var m = Typeload.load_module(ctx, (new Tuple([], "Map")), null_pos);
																var Tuple(a, c) = switch (m.m_types) {
																case ::(TAbstractDecl({ a_impl = Some(c) } = a), _): (new Tuple(a, c));
																case _: assert False;
															};
																var tmap = TAbstract(a, ::(tkey, ::(tval, [])));
																var cf = PMap.find("set", c.cl_statics);
																var v = gen_local(ctx, tmap);
																var ev = mk(TLocal(v), tmap, p);
																var ec = type_module_type(ctx, TClassDecl(c), None, p);
																var ef = mk(TField(ec, FStatic(c, cf)), tfun(::(tkey, ::(tval, [])), ctx.t.tvoid), p);
																var el = ::(ev, List.map2(function e1: function e2: make_call(ctx, ef, ::(ev, ::(e1, ::(e2, []))), ctx.com.basic.tvoid, p), el_k, el_v));
																var enew = mk(TNew(c, ::(tkey, ::(tval, [])), []), tmap, p);
																var el = ::(mk(TVar(v, Some(enew)), t_dynamic, p), List.rev(el));
																mk(TBlock(el), tmap, p);
																case EArrayDecl(el): var tp = switch (with_type) {
																case WithType(t) | WithTypeResume(t): switch (follow(t)) {
																case TInst({ cl_path = ([], Array) }, ::(tp, [])): switch (follow(tp)) {
																case TMono(_): None;
																case _: Some(tp);
															};
																case TAnon(_): try {
																Some(get_iterable_param(t));
															} catch(e:Not_found) {
																None;
															};
																case t: if (==(t, t_dynamic)) {
																Some(t);
															} else {
																None;
															};
															};
																case _: None;
															};
																switch (tp) {
																case None: var el = List.map(function e: type_expr(ctx, e, Value), el);
																var t = try {
																unify_min_raise(ctx, el);
															} catch(e:Error(Unify(l))(p)) {
																if (ctx.untyped) {
																t_dynamic;
															} else {
																display_error(ctx, "Arrays of mixed types are only allowed if the type is forced to Array<Dynamic>", p);
																	raise(Error(Unify(l), p));
																};
																};
																	mk(TArrayDecl(el), ctx.t.tarray(t), p);
																	case Some(t): var el = List.map(function e: var e = type_expr(ctx, e, switch (with_type) {
																	case WithTypeResume(_): WithTypeResume(t);
																	case _: WithType(t);
																});
																	switch (with_type) {
																	case WithTypeResume(_): try {
																	Codegen.AbstractCast.cast_or_unify_raise(ctx, t, e, p);
																} catch(e:Error(Unify(l))(p)) {
																	raise(WithTypeError(l, p));
																};
																	case _: Codegen.AbstractCast.cast_or_unify(ctx, t, e, p);
																}, el);
																	mk(TArrayDecl(el), ctx.t.tarray(t), p);
																};
																	case EVars(vl): type_vars(ctx, vl, p, False);
																	case EFor(it, e2): var Tuple(i, pi, e1) = switch (it) {
																	case (EIn((EConst(Ident(i)), pi), e), _): (new Tuple(i, pi, e));
																	case _: error("For expression should be 'v in expr'", snd(it));
																};
																	var e1 = type_expr(ctx, e1, Value);
																	var old_loop = ctx.in_loop;
																	var old_locals = save_locals(ctx);
																	ctx.in_loop = True;
																	var e = switch (Optimizer.optimize_for_loop(ctx, (new Tuple(i, pi)), e1, e2, p)) {
																	case Some(e): e;
																	case None: var Tuple(t, pt) = Typeload.t_iterator(ctx);
																	var i = add_local(ctx, i, pt);
																	var e1 = switch (follow(e1.etype)) {
																	case TMono(_) | TDynamic(_): display_error(ctx, "You
																	can't iterate on a Dynamic value, please specify Iterator or Iterable", e1.epos);
																	e1;
																	case TLazy(_): assert False;
																	case _: try {
																	Codegen.AbstractCast.cast_or_unify_raise(ctx, t, e1, p);
																} catch(e:Error(Unify(_))(_)) {
																	var acc = build_call(ctx, type_field(ctx, e1, "iterator", e1.epos, MCall), [], Value, e1.epos);
																	try {
																	unify_raise(ctx, acc.etype, t, acc.epos);
																	acc;
																} catch(e:Error(Unify(l))(p)) {
																	display_error(ctx, "Field iterator has an invalid type", acc.epos);
																	display_error(ctx, error_msg(Unify(l)), p);
																	mk(TConst(TNull), t_dynamic, p);
																};
																};
																};
																	var e2 = type_expr(ctx, e2, NoValue);
																	try {
																	Optimizer.optimize_for_loop_iterator(ctx, i, e1, e2, p);
																} catch(e:Exit) {
																	mk(TFor(i, e1, e2), ctx.t.tvoid, p);
																};
																};
																	ctx.in_loop = old_loop;
																	old_locals([]);
																	e;
																	case EIn(_): error("This expression is not allowed outside a for loop", p);
																	case ETernary(e1, e2, e3): type_expr(ctx, (new Tuple(EIf(e1, e2, Some(e3)), p)), with_type);
																	case EIf(e, e1, e2): var e = type_expr(ctx, e, Value);
																	var e = Codegen.AbstractCast.cast_or_unify(ctx, ctx.t.tbool, e, p);
																	var e1 = type_expr(ctx, e1, with_type);
																	switch (e2) {
																	case None: mk(TIf(e, e1, None), ctx.t.tvoid, p);
																	case Some(e2): var e2 = type_expr(ctx, e2, with_type);
																	var Tuple(e1, e2, t) = switch (with_type) {
																	case NoValue: (new Tuple(e1, e2, ctx.t.tvoid));
																	case Value: (new Tuple(e1, e2, unify_min(ctx, ::(e1, ::(e2, [])))));
																	case WithType(t) | WithTypeResume(t) if(switch (follow(t)) {
																	case TMono(_): True;
																	case _: False;
																}): (new Tuple(e1, e2, unify_min(ctx, ::(e1, ::(e2, [])))));
																	case WithType(t) | WithTypeResume(t): try {
																	var e1 = Codegen.AbstractCast.cast_or_unify_raise(ctx, t, e1, e1.epos);
																	var e2 = Codegen.AbstractCast.cast_or_unify_raise(ctx, t, e2, e2.epos);
																	(new Tuple(e1, e2, t));
																} catch(e:Error(Unify(l))(p)) {
																	switch (with_type) {
																	case WithTypeResume(_): raise(WithTypeError(l, p));
																	case _: display_error(ctx, error_msg(Unify(l)), p);
																	(new Tuple(e1, e2, t));
																};
																};
																};
																	mk(TIf(e, e1, Some(e2)), t, p);
																};
																	case EWhile(cond, e, NormalWhile): var old_loop = ctx.in_loop;
																	var cond = type_expr(ctx, cond, Value);
																	var cond = Codegen.AbstractCast.cast_or_unify(ctx, ctx.t.tbool, cond, p);
																	ctx.in_loop = True;
																	var e = type_expr(ctx, e, NoValue);
																	ctx.in_loop = old_loop;
																	mk(TWhile(cond, e, NormalWhile), ctx.t.tvoid, p);
																	case EWhile(cond, e, DoWhile): var old_loop = ctx.in_loop;
																	ctx.in_loop = True;
																	var e = type_expr(ctx, e, NoValue);
																	ctx.in_loop = old_loop;
																	var cond = type_expr(ctx, cond, Value);
																	var cond = Codegen.AbstractCast.cast_or_unify(ctx, ctx.t.tbool, cond, cond.epos);
																	mk(TWhile(cond, e, DoWhile), ctx.t.tvoid, p);
																	case ESwitch(e1, cases, def): try {
																	var dt = match_expr(ctx, e1, cases, def, with_type, p);
																	function wrap(e1) return {
																	if (!(dt.dt_is_complex)) {
																	e1;
																} else {
																	mk(TMeta((new Tuple(Meta.Ast, ::((new Tuple(e, p)), []), p)), e1), e1.etype, e1.epos);
																};
																};
																	wrap(Codegen.PatternMatchConversion.to_typed_ast(ctx, dt, p));
																} catch(e:Exit) {
																	type_switch_old(ctx, e1, cases, def, with_type, p);
																};
																	case EReturn(e): switch (e) {
																	case None: var v = ctx.t.tvoid;
																	unify(ctx, v, ctx.ret, p);
																	mk(TReturn(None), t_dynamic, p);
																	case Some(e): var e = type_expr(ctx, e, WithType(ctx.ret));
																	var e = Codegen.AbstractCast.cast_or_unify(ctx, ctx.ret, e, p);
																	switch (follow(e.etype)) {
																	case TAbstract({ a_path = ([], Void) }, _): mk(TBlock(::(e, ::(mk(TReturn(None), t_dynamic, p), []))), t_dynamic, e.epos);
																	case _: mk(TReturn(Some(e)), t_dynamic, p);
																};
																};
																	case EBreak: if (!(ctx.in_loop)) {
																	display_error(ctx, "Break outside loop", p);
																} else {
																	[];
																};
																	mk(TBreak, t_dynamic, p);
																	case EContinue: if (!(ctx.in_loop)) {
																	display_error(ctx, "Continue outside loop", p);
																} else {
																	[];
																};
																	mk(TContinue, t_dynamic, p);
																	case ETry(e1, []): type_expr(ctx, e1, with_type);
																	case ETry(e1, catches): var e1 = type_expr(ctx, e1, with_type);
																	function check_unreachable(cases, t, p) return {
																	switch (cases) {
																	case ::((v, e), cases): 	function unreachable([]) return {
																	display_error(ctx, "This block is unreachable", p);
																	var st = s_type(print_context([]));
																	display_error(ctx, Printf.sprintf("%s can be assigned to %s, which is handled here", st(t), st(v.v_type)), e.epos);
																};
																	try {
																	switch ((new Tuple(follow(t), follow(v.v_type)))) {
																	case (TDynamic(_), TDynamic(_)): unreachable([]);
																	case (TDynamic(_), _): [];
																	case _: Type.unify(t, v.v_type);
																	unreachable([]);
																};
																} catch(e:Unify_error(_)) {
																	check_unreachable(cases, t, p);
																};
																	case []: [];
																};
																};
																	function check_catch_type(path, params) return {
																	List.iter(function pt: if (!=(pt, t_dynamic)) {
																	error("Catch class parameter must be Dynamic", p);
																} else {
																	[];
																}, params);
																	switch (path) {
																	case (::(x, _), _): x;
																	case ([], name): name;
																};
																};
																	var catches = List.fold_left(function acc: function (v, t, e): var t = Typeload.load_complex_type(ctx, pos(e), t);
																	function loop(t) return {
																	switch (follow(t)) {
																	case TInst({ cl_kind = KTypeParameter(_) } = c, _) if(!(Typeload.is_generic_parameter(ctx, c))): error("Cannot catch non-generic type parameter", p);
																	case TInst({ cl_path = path }, params) | TEnum({ e_path = path }, params): (new Tuple(check_catch_type(path, params), t));
																	case TAbstract(a, params) if(Meta.has(Meta.RuntimeValue, a.a_meta)): (new Tuple(check_catch_type(a.a_path, params), t));
																	case TAbstract(a, tl) if(!(Meta.has(Meta.CoreType, a.a_meta))): loop(Abstract.get_underlying_type(a, tl));
																	case TDynamic(_): (new Tuple("", t));
																	case _: error("Catch type must be a class, an enum or Dynamic", pos(e));
																};
																};
																	var Tuple(name, t2) = loop(t);
																	if (=(v0, '$')) {
																	display_error(ctx, "Catch variable names starting with a dollar are not allowed", p);
																} else {
																	[];
																};
																	check_unreachable(acc, t2, pos(e));
																	var locals = save_locals(ctx);
																	var v = add_local(ctx, v, t);
																	var e = type_expr(ctx, e, with_type);
																	v.v_type = t2;
																	locals([]);
																	if (<>(with_type, NoValue)) {
																	unify(ctx, e.etype, e1.etype, e.epos);
																} else {
																	[];
																};
																	if (PMap.mem(name, ctx.locals)) {
																	error(^("Local variable ", ^(name, " is preventing usage of this type here")), e.epos);
																} else {
																	[];
																};
																	::((new Tuple(v, e)), acc), [], catches);
																	mk(TTry(e1, List.rev(catches)), if (=(with_type, NoValue)) {
																	ctx.t.tvoid;
																} else {
																	e1.etype;
																}, p);
																	case EThrow(e): var e = type_expr(ctx, e, Value);
																	mk(TThrow(e), mk_mono([]), p);
																	case ECall((EConst(Ident(s)), pc) = e, el): try {
																	var Tuple(en, t) = switch (with_type) {
																	case WithType(t) | WithTypeResume(t): switch (follow(t)) {
																	case TEnum(e, pl): (new Tuple(e, t));
																	case _: raise(Exit);
																};
																	case _: raise(Exit);
																};
																	var old = (new Tuple(ctx.on_error, ctx.m.curmod.m_types));
																	ctx.m.curmod.m_types = @(ctx.m.curmod.m_types, ::(TEnumDecl(en), []));
																	function restore([]) return {
																	ctx.m.curmod.m_types = snd(old);
																	ctx.on_error = fst(old);
																};
																	ctx.on_error = function ctx: function msg: function ep: if (=(ep, pc)) {
																	raise(Not_found);
																} else {
																	restore([]);
																	ctx.on_error(ctx, msg, ep);
																};
																	try {
																	var e = type_call(ctx, e, el, with_type, p);
																	restore([]);
																	e;
																} catch(e:T) {
																	McOr(McArr(PaId(IdUid(Not_found)),ExNil,ExSeq(ExSem(ExApp(<...>,<...>),ExSem(<...>,<...>)))),McArr(PaId(IdLid(err)),ExNil,ExSeq(ExSem(ExApp(<...>,<...>),ExApp(<...>,<...>)))))																																			case Not_found: restore([]);
																	if (ctx.untyped) {
																	raise(Exit);
																} else {
																	[];
																};
																	with_type_error(ctx, with_type, string_error(s, en.e_names, ^("Identifier '", ^(s, ^("' is not part of enum ", s_type_path(en.e_path))))), p);
																	mk(TConst(TNull), t, p);
																	case err: restore([]);
																	raise(err);
																};
																} catch(e:Exit) {
																	type_call(ctx, e, el, with_type, p);
																};
																	case ECall(e, el): type_call(ctx, e, el, with_type, p);
																	case ENew(t, el): 	function unify_constructor_call(c, params, f, ct) return {
																	switch (follow(ct)) {
																	case TFun(args, r): try {
																	var Tuple(el, _, _) = unify_field_call(ctx, FInstance(c, params, f), el, args, r, p, False);
																	el;
																} catch(e:Error(e)(p)) {
																	display_error(ctx, error_msg(e), p);
																	[];
																};
																	case _: error("Constructor is not a function", p);
																};
																};
																	var t = if (<>(t.tparams, [])) {
																	follow(Typeload.load_instance(ctx, t, p, False));
																} else {
																	try {
																	ctx.call_argument_stack = ::(el, ctx.call_argument_stack);
																	var t = follow(Typeload.load_instance(ctx, t, p, True));
																	ctx.call_argument_stack = List.tl(ctx.call_argument_stack);
																	switch (t) {
																	case TInst({ cl_kind = KGeneric } = c, tl): follow(Codegen.build_generic(ctx, c, p, tl));
																	case _: t;
																};
																} catch(e:Codegen.Generic_Exception(_)) {
																	switch (Typeload.resolve_typedef(Typeload.load_type_def(ctx, p, t))) {
																	case TClassDecl({ cl_constructor = Some(cf) } = c): var monos = List.map(function _: mk_mono([]), c.cl_params);
																	var Tuple(ct, f) = get_constructor(ctx, c, monos, p);
																	ignore(unify_constructor_call(c, monos, f, ct));
																	try {
																	Codegen.build_generic(ctx, c, p, monos);
																} catch(e:Codegen.Generic_Exception(_) = exc) {
																	switch (with_type) {
																	case WithType(t) | WithTypeResume(t): switch (follow(t)) {
																	case TMono(_): raise(exc);
																	case t: t;
																};
																	case _: raise(exc);
																};
																};
																	case mt: error(^(s_type_path(t_infos(mt).mt_path), " cannot be constructed"), p);
																};
																};
																};
																	function build_constructor_call(c, tl) return {
																	var Tuple(ct, f) = get_constructor(ctx, c, tl, p);
																	if (Meta.has(Meta.CompilerGenerated, f.cf_meta)) {
																	display_error(ctx, ^(s_type_path(c.cl_path), " does not have a constructor"), p);
																} else {
																	[];
																};
																	if (&&(!(||(can_access(ctx, c, f, True), is_parent(c, ctx.curclass))), !(ctx.untyped))) {
																	display_error(ctx, "Cannot access private constructor", p);
																} else {
																	[];
																};
																	switch (f.cf_kind) {
																	case Var({ v_read = AccRequire(r, msg) }): switch (msg) {
																	case Some(msg): error(msg, p);
																	case None: error_require(r, p);
																};
																	case _: [];
																};
																	var el = unify_constructor_call(c, tl, f, ct);
																	(new Tuple(el, f, ct));
																};
																	switch (t) {
																	case TInst({ cl_kind = KTypeParameter(tl) } = c, params): if (!(Typeload.is_generic_parameter(ctx, c))) {
																	error("Only generic type parameters can be constructed", p);
																} else {
																	[];
																};
																	var el = List.map(function e: type_expr(ctx, e, Value), el);
																	var ct = tfun(List.map(function e: e.etype, el), ctx.t.tvoid);
																	if (!(List.exists(function t: switch (follow(t)) {
																	case TAnon(a): try {
																	unify(ctx, PMap.find("new", a.a_fields).cf_type, ct, p);
																	True;
																} catch(e:Not_found) {
																	False;
																};
																	case _: False;
																}, tl))) {
																	error(^(s_type_path(c.cl_path), " does not have a constructor"), p);
																} else {
																	[];
																};
																	mk(TNew(c, params, el), t, p);
																	case TAbstract({ a_impl = Some(c) } = a, tl) if(!(Meta.has(Meta.MultiType, a.a_meta))): var Tuple(el, cf, ct) = build_constructor_call(c, tl);
																	var ta = TAnon({ () with a_fields = c.cl_statics;
																	a_status = ref(Statics(c)) });
																	var e = mk(TTypeExpr(TClassDecl(c)), ta, p);
																	var e = mk(TField(e, FStatic(c, cf)), ct, p);
																	make_call(ctx, e, el, t, p);
																	case TInst(c, params) | TAbstract({ a_impl = Some(c) }, params): var Tuple(el, _, _) = build_constructor_call(c, params);
																	mk(TNew(c, params, el), t, p);
																	case _: error(^(s_type(print_context([]), t), " cannot be constructed"), p);
																};
																	case EUnop(op, flag, e): type_unop(ctx, op, flag, e, p);
																	case EFunction(name, f): var params = Typeload.type_function_params(ctx, f, switch (name) {
																	case None: "localfun";
																	case Some(n): n;
																}, p);
																	if (<>(params, [])) {
																	if (=(name, None)) {
																	display_error(ctx, "Type parameters not supported in unnamed local functions", p);
																} else {
																	[];
																};
																	if (<>(with_type, NoValue)) {
																	error("Type parameters are not supported for rvalue functions", p);
																} else {
																	[];
																};
																} else {
																	[];
																};
																	List.iter(function tp: if (<>(tp.tp_constraints, [])) {
																	display_error(ctx, "Type parameter constraints are not supported for local functions", p);
																} else {
																	[];
																}, f.f_params);
																	var Tuple(inline, v) = switch (name) {
																	case None: (new Tuple(False, None));
																	case Some(v) if(ExtString.String.starts_with(v, "inline_")): (new Tuple(True, Some(String.sub(v, 7, -(String.length(v), 7)))));
																	case Some(v): (new Tuple(False, Some(v)));
																};
																	var Tuple(old_tp, old_in_loop) = (new Tuple(ctx.type_params, ctx.in_loop));
																	ctx.type_params = @(params, ctx.type_params);
																	if (!(inline)) {
																	ctx.in_loop = False;
																} else {
																	[];
																};
																	var rt = Typeload.load_type_opt(ctx, p, f.f_type);
																	var args = List.map(function (s, opt, t, c): var t = Typeload.load_type_opt(ctx, p, t);
																	var Tuple(t, c) = Typeload.type_function_arg(ctx, t, c, opt, p);
																	(new Tuple(s, c, t)), f.f_args);
																	switch (with_type) {
																	case WithType(t) | WithTypeResume(t): 	function loop(t) return {
																	switch (follow(t)) {
																	case TFun(args2, tr) if(=(List.length(args2), List.length(args))): List.iter2(function (_, _, t1): function (_, _, t2): switch (follow(t1)) {
																	case TMono(_): unify(ctx, t2, t1, p);
																	case _: [];
																}, args, args2);
																	switch ((new Tuple(follow(tr), follow(rt)))) {
																	case (TAbstract({ a_path = ([], Void) }, _), _): [];
																	case (_, TMono(_)): unify(ctx, rt, tr, p);
																	case _: [];
																};
																	case TAbstract(a, tl): loop(Abstract.get_underlying_type(a, tl));
																	case _: [];
																};
																};
																	loop(t);
																	case NoValue: if (=(name, None)) {
																	display_error(ctx, "Unnamed lvalue functions are not supported", p);
																} else {
																	[];
																};
																	case _: [];
																};
																	var ft = TFun(fun_args(args), rt);
																	var v = switch (v) {
																	case None: None;
																	case Some(v): if (=(v0, '$')) {
																	display_error(ctx, "Variable names starting with a dollar are not allowed", p);
																} else {
																	[];
																};
																	Some(add_local(ctx, v, ft));
																};
																	var curfun = switch (ctx.curfun) {
																	case FunStatic: FunStatic;
																	case FunMemberAbstract: FunMemberAbstractLocal;
																	case _: FunMemberClassLocal;
																};
																	var Tuple(e, fargs) = Typeload.type_function(ctx, args, rt, curfun, f, False, p);
																	ctx.type_params = old_tp;
																	ctx.in_loop = old_in_loop;
																	var f = { () with tf_args = fargs;
																	tf_type = rt;
																	tf_expr = e };
																	var e = mk(TFunction(f), ft, p);
																	switch (v) {
																	case None: e;
																	case Some(v): if (||(<>(params, []), inline)) {
																	v.v_extra = Some(params, if (inline) {
																	Some(e);
																} else {
																	None;
																});
																} else {
																	[];
																};
																	function loop(match) return switch (match) {
																	case Filters.Block(f) | Filters.Loop(f) | Filters.Function(f): f(loop);
																	case Filters.Use(v2) | Filters.Assign(v2) if(==(v, v2)): raise(Exit);
																	case Filters.Use(_) | Filters.Assign(_) | Filters.Declare(_): [];
																};
																	var is_rec = try {
																	Filters.local_usage(loop, e);
																	False;
																} catch(e:Exit) {
																	True;
																};
																	var decl = if (is_rec) {
																	if (inline) {
																	display_error(ctx, "Inline function cannot be recursive", e.epos);
																} else {
																	[];
																};
																	var vnew = add_local(ctx, v.v_name, ft);
																	mk(TVar(vnew, Some(mk(TBlock(::(mk(TVar(v, Some(mk(TConst(TNull), ft, p))), ctx.t.tvoid, p), ::(mk(TBinop(OpAssign, mk(TLocal(v), ft, p), e), ft, p), ::(mk(TLocal(v), ft, p), [])))), ft, p))), ctx.t.tvoid, p);
																} else {
																	if (inline) {
																	mk(TBlock([]), ctx.t.tvoid, p);
																} else {
																	mk(TVar(v, Some(e)), ctx.t.tvoid, p);
																};
																};
																	if (&&(<>(with_type, NoValue), !(inline))) {
																	mk(TBlock(::(decl, ::(mk(TLocal(v), v.v_type, p), []))), v.v_type, p);
																} else {
																	decl;
																};
																};
																	case EUntyped(e): var old = ctx.untyped;
																	ctx.untyped = True;
																	if (!(Meta.has(Meta.HasUntyped, ctx.curfield.cf_meta))) {
																	ctx.curfield.cf_meta = ::((new Tuple(Meta.HasUntyped, [], p)), ctx.curfield.cf_meta);
																} else {
																	[];
																};
																	var e = type_expr(ctx, e, with_type);
																	ctx.untyped = old;
																{ () with eexpr = e.eexpr;
																	etype = mk_mono([]);
																	epos = e.epos };
																	case ECast(e, None): var e = type_expr(ctx, e, Value);
																	mk(TCast(e, None), mk_mono([]), p);
																	case ECast(e, Some(t)): var t = Typeload.load_complex_type(ctx, pos(e), t);
																	function loop(t) return {
																	switch (follow(t)) {
																	case TInst(_, params) | TEnum(_, params): List.iter(function pt: if (!=(follow(pt), t_dynamic)) {
																	error("Cast type parameters must be Dynamic", p);
																} else {
																	[];
																}, params);
																	switch (follow(t)) {
																	case TInst(c, _): switch (c.cl_kind) {
																	case KTypeParameter(_): error("Can't cast to a type parameter", p);
																	case _: [];
																};
																	TClassDecl(c);
																	case TEnum(e, _): TEnumDecl(e);
																	case _: assert False;
																};
																	case TAbstract(a, params) if(Meta.has(Meta.RuntimeValue, a.a_meta)): List.iter(function pt: if (!=(follow(pt), t_dynamic)) {
																	error("Cast type parameters must be Dynamic", p);
																} else {
																	[];
																}, params);
																	TAbstractDecl(a);
																	case TAbstract(a, params): loop(Abstract.get_underlying_type(a, params));
																	case _: error("Cast type must be a class or an enum", p);
																	};
																	};
																		var texpr = loop(t);
																		mk(TCast(type_expr(ctx, e, Value), Some(texpr)), t, p);
																		case EDisplay(e, iscall): handle_display(ctx, e, iscall, with_type, p);
																		case EDisplayNew(t): var t = Typeload.load_instance(ctx, t, p, True);
																		switch (follow(t)) {
																		case TInst(c, params) | TAbstract({ a_impl = Some(c) }, params): var Tuple(ct, f) = get_constructor(ctx, c, params, p);
																		raise(DisplayTypes(::(ct, List.map(function f: f.cf_type, f.cf_overloads))));
																		case _: error("Not a class", p);
																		};
																			case ECheckType(e, t): var t = Typeload.load_complex_type(ctx, p, t);
																			var e = type_expr(ctx, e, WithType(t));
																			var e = Codegen.AbstractCast.cast_or_unify(ctx, t, e, p);
																			if (==(e.etype, t)) {
																			e;
																		} else {
																			mk(TCast(e, None), t, p);
																		};
																			case EMeta(m, e1): var old = ctx.meta;
																			ctx.meta = ::(m, ctx.meta);
																			function e([]) return {
																			type_expr(ctx, e1, with_type);
																		};
																			var e = switch (m) {
																			case (Meta.ToString, _, _): var e = e([]);
																			switch (follow(e.etype)) {
																			case TAbstract({ a_impl = Some(c) }, _) if(PMap.mem("toString", c.cl_statics)): call_to_string(ctx, c, e);
																			case _: e;
																		};
																			case (Meta.This, _, _): var e = List.hd(ctx.this_stack);
																			function loop(e) return {
																			switch (e.eexpr) {
																			case TConst(TThis): get_this(ctx, e.epos);
																			case _: Type.map_expr(loop, e);
																		};
																		};
																			loop(e);
																			case (Meta.Analyzer, _, _): var e = e([]);
																		{ (e) with eexpr = TMeta(m, e) };
																			case (Meta.MergeBlock, _, _): switch (fst(e1)) {
																			case EBlock(el): type_block(ctx, el, with_type, p);
																			case _: e([]);
																		};
																			case (Meta.StoredTypedExpr, _, _): var id = switch (e1) {
																			case (EConst(Int(s)), _): int_of_string(s);
																			case _: assert False;
																		};
																			get_stored_typed_expr(ctx.com, id);
																			case (Meta.NoPrivateAccess, _, _): ctx.meta = List.filter(function (m, _, _): <>(m, Meta.PrivateAccess), ctx.meta);
																			e([]);
																			case _: e([]);
																		};
																			ctx.meta = old;
																			e;
																		};
																		};

																			public static var get_next_stored_typed_expr_id = var uid = ref(0);
																			function []: incr(uid);
																			uid.val;

																			public static function get_stored_typed_expr(com, id) return {
																			var vars = Hashtbl.create(0);
																			function copy_var(v) return {
																			var v2 = alloc_var(v.v_name, v.v_type);
																			v2.v_meta = v.v_meta;
																			Hashtbl.add(vars, v.v_id, v2);
																			v2;
																		};
																			function build_expr(e) return {
																			switch (e.eexpr) {
																			case TVar(v, eo): var v2 = copy_var(v);
																		{ (e) with eexpr = TVar(v2, Option.map(build_expr, eo)) };
																			case TFor(v, e1, e2): var v2 = copy_var(v);
																		{ (e) with eexpr = TFor(v2, build_expr(e1), build_expr(e2)) };
																			case TTry(e1, cl): var cl = List.map(function (v, e): var v2 = copy_var(v);
																			(new Tuple(v2, build_expr(e))), cl);
																		{ (e) with eexpr = TTry(build_expr(e1), cl) };
																			case TFunction(f): var args = List.map(function (v, c): (new Tuple(copy_var(v), c)), f.tf_args);
																			var f = { () with tf_args = args;
																			tf_type = f.tf_type;
																			tf_expr = build_expr(f.tf_expr) };
																		{ (e) with eexpr = TFunction(f) };
																			case TLocal(v): try {
																			var v2 = Hashtbl.find(vars, v.v_id);
																		{ (e) with eexpr = TLocal(v2) };
																		} catch(e:_) {
																			e;
																		};
																			case _: map_expr(build_expr, e);
																		};
																		};
																			var e = PMap.find(id, com.stored_typed_exprs);
																			build_expr(e);
																		};

																			public static function handle_display(ctx, e_ast, iscall, with_type, p) return {
																			var old = ctx.in_display;
																			ctx.in_display = True;
																			function get_submodule_fields(path) return {
																			var m = Hashtbl.find(ctx.g.modules, path);
																			var tl = List.filter(function t: &&(<>(path, t_infos(t).mt_path), !(t_infos(t).mt_private)), m.m_types);
																			var tl = List.map(function mt: var infos = t_infos(mt);
																			(new Tuple(snd(infos.mt_path), type_of_module_type(mt), Some(FKType), infos.mt_doc)), tl);
																			tl;
																		};
																			var e = try {
																			type_expr(ctx, e_ast, Value);
																		} catch(e:T) {
																			McOr(McArr(PaApp(PaApp(PaId(<...>),PaApp(<...>,<...>)),PaAny),ExApp(ExId(IdLid(<...>)),ExId(IdLid(<...>))),ExApp(ExId(IdLid(<...>)),ExApp(ExApp(<...>,<...>),ExId(<...>)))),McOr(McArr(PaApp(PaApp(<...>,<...>),PaAny),ExNil,ExApp(ExId(<...>),ExApp(<...>,<...>))),McArr(PaAli(PaApp(<...>,<...>),PaId(<...>)),ExNil,ExTry(ExApp(<...>,<...>),McArr(<...>,<...>,<...>)))))			case Error(Unknown_ident(n), _) if(!(iscall)): raise(Parser.TypePath(::(n, []), None, False));
																			case Error(Unknown_ident(trace), _): raise(DisplayTypes(::(tfun(::(t_dynamic, []), ctx.com.basic.tvoid), [])));
																			case Error(Type_not_found(path, _), _) = err: try {
																			raise(DisplayFields(get_submodule_fields(path)));
																		} catch(e:Not_found) {
																			raise(err);
																		};
																		};
																			ctx.in_display = old;
																			function handle_field(cf) return {
																			if (=(ctx.com.display, DMPosition)) {
																			raise(DisplayPosition(::(cf.cf_pos, [])));
																		} else {
																			[];
																		};
																			cf.cf_meta = ::((new Tuple(Meta.Usage, [], p)), cf.cf_meta);
																		};
																			switch (ctx.com.display) {
																			case DMResolve(_): assert False;
																			case DMType: raise(DisplayTypes(::(e.etype, [])));
																			case DMUsage | DMPosition: switch (e.eexpr) {
																			case TField(_, FEnum(_, ef)): if (=(ctx.com.display, DMPosition)) {
																			raise(DisplayPosition(::(ef.ef_pos, [])));
																		} else {
																			[];
																		};
																			ef.ef_meta = ::((new Tuple(Meta.Usage, [], p)), ef.ef_meta);
																			case TField(_, FAnon(cf) | FInstance(_, _, cf) | FStatic(_, cf) | FClosure(_, cf)): handle_field(cf);
																			case TLocal(v): v.v_meta = ::((new Tuple(Meta.Usage, [], p)), v.v_meta);
																			case TTypeExpr(mt): var ti = t_infos(mt);
																			if (=(ctx.com.display, DMPosition)) {
																			raise(DisplayPosition(::(ti.mt_pos, [])));
																		} else {
																			[];
																		};
																			ti.mt_meta = ::((new Tuple(Meta.Usage, [], p)), ti.mt_meta);
																			case TNew(c, tl, _): try {
																			var Tuple(_, cf) = get_constructor(ctx, c, tl, p);
																			handle_field(cf);
																		} catch(e:Not_found) {
																			[];
																		};
																			case _: [];
																		};
																			e;
																			case DMToplevel: collect_toplevel_identifiers(ctx);
																			case DMDefault | DMNone: 	function opt_args(args, ret) return {
																			TFun(List.map(function (n, o, t): (new Tuple(n, True, t)), args), ret);
																		};
																			var e = switch (e.eexpr) {
																			case TField(e1, fa): if (=(field_name(fa), "bind")) {
																			switch (follow(e1.etype)) {
																			case TFun(args, ret): { (e1) with etype = opt_args(args, ret) };
																			case _: e;
																		};
																		} else {
																			e;
																		};
																			case _: e;
																		};
																			function opt_type(t) return {
																			switch (t) {
																			case TLazy(f): Typeload.return_partial_type.val = True;
																			var t = f.val([]);
																			Typeload.return_partial_type.val = False;
																			t;
																			case _: t;
																		};
																		};
																			function merge_core_doc(c) return {
																			var c_core = Typeload.load_core_class(ctx, c);
																			if (=(c.cl_doc, None)) {
																			c.cl_doc = c_core.cl_doc;
																		} else {
																			[];
																		};
																			function maybe_merge(cf_map, cf) return {
																			if (=(cf.cf_doc, None)) {
																			try {
																			cf.cf_doc = PMap.find(cf.cf_name, cf_map).cf_doc;
																		} catch(e:Not_found) {
																			[];
																		};
																		} else {
																			[];
																		};
																		};
																			List.iter(maybe_merge(c_core.cl_fields), c.cl_ordered_fields);
																			List.iter(maybe_merge(c_core.cl_statics), c.cl_ordered_statics);
																			switch ((new Tuple(c.cl_constructor, c_core.cl_constructor))) {
																			case (Some({ cf_doc = None } = cf), Some(cf2)): cf.cf_doc = cf2.cf_doc;
																			case _: [];
																		};
																		};
																			function get_fields(t) return {
																			switch (follow(t)) {
																			case TInst(c, params): if (Meta.has(Meta.CoreApi, c.cl_meta)) {
																			merge_core_doc(c);
																		} else {
																			[];
																		};
																			var priv = is_parent(c, ctx.curclass);
																			function merge(?:(cond = function _: True), a, b) return {
																			PMap.foldi(function k: function f: function m: if (cond(f)) {
																			PMap.add(k, f, m);
																		} else {
																			m;
																		}, a, b);
																		};
																			function loop(c, params) return {
																			var m = List.fold_left(function m: function (i, params): merge(m, loop(i, params)), PMap.empty, c.cl_implements);
																			var m = switch (c.cl_super) {
																			case None: m;
																			case Some(csup, cparams): merge(m, loop(csup, cparams));
																		};
																			var m = merge(cond = function f: ||(priv, can_access(ctx, c, f, False)), c.cl_fields, m);
																			var m = switch (c.cl_kind) {
																			case KTypeParameter(pl): List.fold_left(function acc: function t: merge(acc, get_fields(t)), m, pl);
																			case _: m;
																		};
																			PMap.map(function f: { (f) with cf_type = apply_params(c.cl_params, params, opt_type(f.cf_type));
																			cf_public = True }, m);
																		};
																			loop(c, params);
																			case TAbstract({ a_impl = Some(c) } = a, pl): if (Meta.has(Meta.CoreApi, c.cl_meta)) {
																			merge_core_doc(c);
																		} else {
																			[];
																		};
																			ctx.m.module_using = ::(c, ctx.m.module_using);
																			var fields = try {
																			var Tuple(_, el, _) = Meta.get(Meta.Forward, a.a_meta);
																			var sl = ExtList.List.filter_map(function e: switch (fst(e)) {
																			case EConst(Ident(s)): Some(s);
																			case _: None;
																		}, el);
																			var fields = get_fields(apply_params(a.a_params, pl, a.a_this));
																			if (=(sl, [])) {
																			fields;
																		} else {
																			PMap.fold(function cf: function acc: if (List.mem(cf.cf_name, sl)) {
																			PMap.add(cf.cf_name, cf, acc);
																		} else {
																			acc;
																		}, fields, PMap.empty);
																		};
																		} catch(e:Not_found) {
																			PMap.empty;
																		};
																			PMap.fold(function f: function acc: if (&&(<>(f.cf_name, "_new"), &&(can_access(ctx, c, f, True), &&(Meta.has(Meta.Impl, f.cf_meta), !(Meta.has(Meta.Enum, f.cf_meta)))))) {
																			var f = prepare_using_field(f);
																			var t = apply_params(a.a_params, pl, follow(f.cf_type));
																			PMap.add(f.cf_name, { (f) with cf_public = True;
																			cf_type = opt_type(t) }, acc);
																		} else {
																			acc;
																		}, c.cl_statics, fields);
																			case TAnon(a) if(PMap.is_empty(a.a_fields)): switch (with_type) {
																			case WithType(t) | WithTypeResume(t): get_fields(t);
																			case _: a.a_fields;
																		};
																			case TAnon(a): switch (a.a_status.val) {
																			case Statics(c): if (Meta.has(Meta.CoreApi, c.cl_meta)) {
																			merge_core_doc(c);
																		} else {
																			[];
																		};
																			var is_abstract_impl = switch (c.cl_kind) {
																			case KAbstractImpl(_): True;
																			case _: False;
																		};
																			var pm = switch (c.cl_constructor) {
																			case None: PMap.empty;
																			case Some(cf): PMap.add("new", cf, PMap.empty);
																		};
																			PMap.fold(function f: function acc: if (&&(can_access(ctx, c, f, True), ||(!(is_abstract_impl), ||(!(Meta.has(Meta.Impl, f.cf_meta)), Meta.has(Meta.Enum, f.cf_meta))))) {
																			PMap.add(f.cf_name, { (f) with cf_public = True;
																			cf_type = opt_type(f.cf_type) }, acc);
																		} else {
																			acc;
																		}, a.a_fields, pm);
																			case _: a.a_fields;
																		};
																			case TFun(args, ret): var t = opt_args(args, ret);
																			var cf = mk_field("bind", tfun(::(t, []), t), p);
																			PMap.add("bind", cf, PMap.empty);
																			case _: PMap.empty;
																		};
																		};
																			var fields = get_fields(e.etype);
																			function loop(acc) return {
																			case []: acc;
																			case ::(c, l): var acc = ref(loop(acc, l));
																			function dup(t) return {
																			Type.map(dup, t);
																		};
																			List.iter(function f: if (!(Meta.has(Meta.NoUsing, f.cf_meta))) {
																			var f = { (f) with cf_type = opt_type(f.cf_type) };
																			var monos = List.map(function _: mk_mono([]), f.cf_params);
																			var map = apply_params(f.cf_params, monos);
																			switch (follow(map(f.cf_type))) {
																			case TFun(::((_, _, TType({ t_path = (::(haxe, ::(macro, [])), ExprOf) }, ::(t, []))), args), ret) | TFun(::((_, _, t), args), ret): try {
																			unify_raise(ctx, dup(e.etype), t, e.epos);
																			List.iter2(function m: function (name, t): switch (follow(t)) {
																			case TInst({ cl_kind = KTypeParameter(constr) }, _) if(<>(constr, [])): List.iter(function tc: unify_raise(ctx, m, map(tc), e.epos), constr);
																			case _: [];
																		}, monos, f.cf_params);
																			if (||(!(can_access(ctx, c, f, True)), &&(==(follow(e.etype), t_dynamic), !=(follow(t), t_dynamic)))) {
																			[];
																		} else {
																			var f = prepare_using_field(f);
																			var f = { (f) with cf_params = [];
																			cf_public = True;
																			cf_type = TFun(args, ret) };
																			acc.val = PMap.add(f.cf_name, f, acc.val);
																		};
																		} catch(e:Error(Unify(_))(_)) {
																			[];
																		};
																			case _: [];
																		};
																		} else {
																			[];
																		}, c.cl_ordered_statics);
																			acc.val;
																		};
																			var use_methods = switch (follow(e.etype)) {
																			case TMono(_): PMap.empty;
																			case _: loop(loop(PMap.empty, ctx.g.global_using), ctx.m.module_using);
																		};
																			var fields = PMap.fold(function f: function acc: PMap.add(f.cf_name, f, acc), fields, use_methods);
																			var fields = switch (fst(e_ast)) {
																			case EConst(String(s)) if(=(String.length(s), 1)): var cf = mk_field("code", ctx.t.tint, e.epos);
																			cf.cf_doc = Some("The character code of this character [inlined at compile - time].");
																				cf.cf_kind = Var({ () with v_read = AccNormal;
																				v_write = AccNever });
																				PMap.add(cf.cf_name, cf, fields);
																				case _: fields;
																			};
																				var fields = PMap.fold(function f: function acc: if (Meta.has(Meta.NoCompletion, f.cf_meta)) {
																				acc;
																			} else {
																				::(f, acc);
																			}, fields, []);
																				var t = if (iscall) {
																				function loop(t) return {
																				switch (follow(t)) {
																				case TFun(_): t;
																				case TAbstract(a, tl) if(Meta.has(Meta.Callable, a.a_meta)): loop(Abstract.get_underlying_type(a, tl));
																				case _: t_dynamic;
																			};
																			};
																				loop(e.etype);
																			} else {
																				function get_field(acc, f) return {
																				List.fold_left(function acc: function f: var kind = switch (f.cf_kind) {
																				case Method(_): FKMethod;
																				case Var(_): FKVar;
																			};
																				if (f.cf_public) {
																				::((new Tuple(f.cf_name, f.cf_type, Some(kind), f.cf_doc)), acc);
																			} else {
																				acc;
																			}, acc, ::(f, f.cf_overloads));
																			};
																				var fields = List.fold_left(get_field, [], fields);
																				var fields = try {
																				var sl = string_list_of_expr_path_raise(e_ast);
																				@(fields, get_submodule_fields((new Tuple(List.tl(sl), List.hd(sl)))));
																			} catch(e:Exit | Not_found) {
																				fields;
																			};
																				if (=(fields, [])) {
																				e.etype;
																			} else {
																				raise(DisplayFields(fields));
																			};
																			};
																				switch (follow(t)) {
																				case TMono(_) | TDynamic(_) if(ctx.in_macro): mk(TConst(TNull), t, p);
																				case _: raise(DisplayTypes(::(t, [])));
																			};
																			};
																			};

																				public static function type_call(ctx, e, el, with_typewith_type, p) return {
																				function def([]) return {
																				switch (e) {
																				case (EField((EConst(Ident(super)), _), _), _): ctx.in_super_call = True;
																				case _: [];
																			};
																				build_call(ctx, type_access(ctx, fst(e), snd(e), MCall), el, with_type, p);
																			};
																				switch ((new Tuple(e, el))) {
																				case ((EConst(Ident(trace)), p), ::(e, el)): if (Common.defined(ctx.com, Define.NoTraces)) {
																				null(ctx.t.tvoid, p);
																			} else {
																				var params = switch (el) {
																				case []: [];
																				case _: ::((new Tuple("customParams", (new Tuple(EArrayDecl(el), p)))), []);
																			};
																				var infos = mk_infos(ctx, p, params);
																				if (&&(||(platform(ctx.com, Js), platform(ctx.com, Python)), &&(=(el, []), has_dce(ctx.com)))) {
																				var e = type_expr(ctx, e, Value);
																				var infos = type_expr(ctx, infos, Value);
																				var e = switch (follow(e.etype)) {
																				case TAbstract({ a_impl = Some(c) }, _) if(PMap.mem("toString", c.cl_statics)): call_to_string(ctx, c, e);
																				case _: e;
																			};
																				var v_trace = alloc_unbound_var("`trace", t_dynamic);
																				mk(TCall(mk(TLocal(v_trace), t_dynamic, p), ::(e, ::(infos, []))), ctx.t.tvoid, p);
																			} else {
																				var me = (new Tuple(Meta.ToString, [], pos(e)));
																				type_expr(ctx, (new Tuple(ECall((new Tuple(EField((new Tuple(EField((new Tuple(EConst(Ident("haxe")), p)), "Log"), p)), "trace"), p)), ::((new Tuple(EMeta(me, e), pos(e))), ::(infos, []))), p)), NoValue);
																			};
																			};
																				case ((EConst(Ident(callback)), p1), args): var ecb = try {
																				Some(type_ident_raise(ctx, "callback", p1, MCall));
																			} catch(e:Not_found) {
																				None;
																			};
																				switch (ecb) {
																				case Some(ecb): build_call(ctx, ecb, args, with_type, p);
																				case None: display_error(ctx, "callback syntax has changed to func.bind[args]", p);
																				var e = type_expr(ctx, e, Value);
																				type_bind(ctx, e, args, p);
																			};
																				case ((EField((EConst(Ident(super)), _), _), _), _): def([]);
																				case ((EField(e, bind), p), args): var e = type_expr(ctx, e, Value);
																				switch (follow(e.etype)) {
																				case TFun(_): type_bind(ctx, e, args, p);
																				case _: def([]);
																			};
																				case ((EConst(Ident($type)), _), ::(e, [])): var e = type_expr(ctx, e, Value);
																				ctx.com.warning(s_type(print_context([]), e.etype), e.epos);
																				e;
																				case ((EField(e, match), p), ::(epat, [])): var et = type_expr(ctx, e, Value);
																				switch (follow(et.etype)) {
																				case TEnum(_) = t: var e = match_expr(ctx, e, ::((new Tuple(::(epat, []), None, Some(EConst(Ident("true")), p))), []), Some(Some(EConst(Ident("false")), p)), WithType(ctx.t.tbool), p);
																				var locals = get_pattern_locals_ref.val(ctx, epat, t);
																				PMap.iter(function _: function (_, p): display_error(ctx, "Capture variables are not allowed", p), locals);
																				Codegen.PatternMatchConversion.to_typed_ast(ctx, e, p);
																				case _: def([]);
																			};
																				case ((EConst(Ident(__unprotect__)), _), ::((EConst(String(_)), _) = e, [])): var e = type_expr(ctx, e, Value);
																				if (Common.platform(ctx.com, Flash)) {
																				var t = tfun(::(e.etype, []), e.etype);
																				var v_unprotect = alloc_unbound_var("__unprotect__", t);
																				mk(TCall(mk(TLocal(v_unprotect), t, p), ::(e, [])), e.etype, e.epos);
																			} else {
																				e;
																			};
																				case ((EConst(Ident(super)), sp), el): if (<>(ctx.curfun, FunConstructor)) {
																				error("Cannot call super constructor outside class constructor", p);
																				} else {
																					[];
																				};
																					var Tuple(el, t) = switch (ctx.curclass.cl_super) {
																					case None: error("Current class does not have a super", p);
																						case Some(c, params): var Tuple(ct, f) = get_constructor(ctx, c, params, p);
																						if (Meta.has(Meta.CompilerGenerated, f.cf_meta)) {
																						display_error(ctx, ^(s_type_path(c.cl_path), " does not have a constructor"), p);
																					} else {
																						[];
																					};
																						var el = switch (follow(ct)) {
																						case TFun(args, r): var Tuple(el, _, _) = unify_field_call(ctx, FInstance(c, params, f), el, args, r, p, False);
																						el;
																						case _: error("Constructor is not a function", p);
																					};
																						(new Tuple(el, TInst(c, params)));
																					};
																						mk(TCall(mk(TConst(TSuper), t, sp), el), ctx.t.tvoid, p);
																						case _: def([]);
																					};
																					};

																						public static function build_call(ctx, acc, el, with_typewith_type, p) return {
																						switch (acc) {
																						case AKInline(ethis, f, fmode, t) if(Meta.has(Meta.Generic, f.cf_meta)): type_generic_function(ctx, (new Tuple(ethis, fmode)), el, with_type, p);
																						case AKInline(ethis, f, fmode, t): switch (follow(t)) {
																						case TFun(args, r): var Tuple(_, _, mk_call) = unify_field_call(ctx, fmode, el, args, r, p, True);
																						mk_call(ethis, p);
																						case _: error(^(s_type(print_context([]), t), " cannot be called"), p);
																					};
																						case AKUsing(et, cl, ef, eparam) if(Meta.has(Meta.Generic, ef.cf_meta)): switch (et.eexpr) {
																						case TField(ec, fa): type_generic_function(ctx, (new Tuple(ec, fa)), el, using_param = Some(eparam), with_type, p);
																						case _: assert False;
																					};
																						case AKUsing(et, cl, ef, eparam): switch (ef.cf_kind) {
																						case Method(MethMacro): var ethis = type_module_type(ctx, TClassDecl(cl), None, p);
																						var Tuple(eparam, f) = Codegen.push_this(ctx, eparam);
																						var e = build_call(ctx, AKMacro(ethis, ef), ::(eparam, el), with_type, p);
																						f([]);
																						e;
																						case _: var t = follow(field_type(ctx, cl, [], ef, p));
																						var Tuple(t, tthis) = switch (follow(eparam.etype)) {
																						case TAbstract(a, tl) if(Meta.has(Meta.Impl, ef.cf_meta)): (new Tuple(apply_params(a.a_params, tl, t), apply_params(a.a_params, tl, a.a_this)));
																						case te: (new Tuple(t, te));
																					};
																						var Tuple(params, args, r, eparam) = switch (t) {
																						case TFun(::((_, _, t1), args), r): unify(ctx, tthis, t1, eparam.epos);
																						var ef = prepare_using_field(ef);
																						switch (unify_call_args(ctx, el, args, r, p, =(ef.cf_kind, Method(MethInline)), is_forced_inline(Some(cl), ef))) {
																						case (el, TFun(args, r)): (new Tuple(el, args, r, eparam));
																						case _: assert False;
																					};
																						case _: assert False;
																					};
																						make_call(ctx, et, ::(eparam, params), r, p);
																					};
																						case AKMacro(ethis, cf): if (>(ctx.macro_depth, 300)) {
																						error("Stack overflow", p);
																					} else {
																						[];
																					};
																						ctx.macro_depth = +(ctx.macro_depth, 1);
																						ctx.with_type_stack = ::(with_type, ctx.with_type_stack);
																						var ethis_f = ref(function []: []);
																						var f = switch (ethis.eexpr) {
																						case TTypeExpr(TClassDecl(c)): switch (ctx.g.do_macro(ctx, MExpr, c.cl_path, cf.cf_name, el, p)) {
																						case None: function []: type_expr(ctx, (new Tuple(EConst(Ident("null")), p)), Value);
																						case Some(EMeta((Meta.MergeBlock, _, _), (EBlock(el), _)), _): function []: var e = type_block(ctx, el, with_type, p);
																						mk(TMeta((new Tuple(Meta.MergeBlock, [], p)), e), e.etype, e.epos);
																						case Some(EVars(vl), p): function []: type_vars(ctx, vl, p, True);
																						case Some(e): function []: type_expr(ctx, e, with_type);
																					};
																						case _: switch (follow(ethis.etype)) {
																						case TInst(c, _): 	function loop(c) return {
																						if (PMap.mem(cf.cf_name, c.cl_fields)) {
																						var Tuple(eparam, f) = Codegen.push_this(ctx, ethis);
																						ethis_f.val = f;
																						var e = switch (ctx.g.do_macro(ctx, MExpr, c.cl_path, cf.cf_name, ::(eparam, el), p)) {
																						case None: function []: type_expr(ctx, (new Tuple(EConst(Ident("null")), p)), Value);
																						case Some(e): function []: type_expr(ctx, e, Value);
																					};
																						e;
																					} else {
																						switch (c.cl_super) {
																						case None: assert False;
																						case Some(csup, _): loop(csup);
																					};
																					};
																					};
																						loop(c);
																						case _: assert False;
																					};
																					};
																						ctx.macro_depth = -(ctx.macro_depth, 1);
																						ctx.with_type_stack = List.tl(ctx.with_type_stack);
																						var old = ctx.on_error;
																						ctx.on_error = function ctx: function msg: function ep: if (||(<>(ep.pfile, p.pfile), ||(<(ep.pmax, p.pmin), >(ep.pmin, p.pmax)))) {
																						Typeload.locate_macro_error.val = False;
																						old(ctx, msg, ep);
																						Typeload.locate_macro_error.val = True;
																						ctx.com.error("Called from macro here", p);
																					} else {
																						old(ctx, msg, ep);
																					};
																						var e = try {
																						f([]);
																					} catch(e:Error(m)(p)) {
																						ctx.on_error = old;
																						ethis_f.val([]);
																						raise(Fatal_error(error_msg(m), p));
																					};
																						ctx.on_error = old;
																						ethis_f.val([]);
																						e;
																						case AKNo(_) | AKSet(_) | AKAccess(_): ignore(acc_get(ctx, acc, p));
																						assert False;
																						case AKExpr(e): 	function loop(t) return {
																						switch (follow(t)) {
																						case TFun(args, r): switch (e.eexpr) {
																						case TField(e1, fa) if(!(switch (fa) {
																						case FEnum(_): True;
																						case _: False;
																					})): switch (fa) {
																						case FInstance(_, _, cf) | FStatic(_, cf) if(Meta.has(Meta.Generic, cf.cf_meta)): type_generic_function(ctx, (new Tuple(e1, fa)), el, with_type, p);
																						case _: var Tuple(_, _, mk_call) = unify_field_call(ctx, fa, el, args, r, p, False);
																						mk_call(e1, e.epos);
																					};
																						case _: var Tuple(el, tfunc) = unify_call_args(ctx, el, args, r, p, False, False);
																						var r = switch (tfunc) {
																						case TFun(_, r): r;
																						case _: assert False;
																					};
																						mk(TCall({ (e) with etype = tfunc }, el), r, p);
																					};
																						case TAbstract(a, tl) if(Meta.has(Meta.Callable, a.a_meta)): loop(Abstract.get_underlying_type(a, tl));
																						case TMono(_): var t = mk_mono([]);
																						var el = List.map(function e: type_expr(ctx, e, Value), el);
																						unify(ctx, tfun(List.map(function e: e.etype, el), t), e.etype, e.epos);
																						mk(TCall(e, el), t, p);
																						case t: var el = List.map(function e: type_expr(ctx, e, Value), el);
																						var t = if (==(t, t_dynamic)) {
																						t_dynamic;
																					} else {
																						if (ctx.untyped) {
																						mk_mono([]);
																					} else {
																						error(^(s_type(print_context([]), e.etype), " cannot be called"), e.epos);
																					};
																					};
																						mk(TCall(e, el), t, p);
																					};
																					};
																						loop(e.etype);
																					};
																					};

																						public static function get_main(ctx) return {
																						switch (ctx.com.main_class) {
																						case None: None;
																						case Some(cl): var t = Typeload.load_type_def(ctx, null_pos, { () with tpackage = fst(cl);
																						tname = snd(cl);
																						tparams = [];
																						tsub = None });
																						var Tuple(fmode, ft, r) = switch (t) {
																						case TEnumDecl(_) | TTypeDecl(_) | TAbstractDecl(_): error(^("Invalid - main : ", ^(s_type_path(cl), " is not a
																						class")), null_pos);
																							case TClassDecl(c): try {
																							var f = PMap.find("main", c.cl_statics);
																							var t = Type.field_type(f);
																							switch (follow(t)) {
																							case TFun([], r): (new Tuple(FStatic(c, f), t, r));
																							case _: error(^("Invalid - main : ", ^(s_type_path(cl), " has invalid main function")), c.cl_pos);
																						};
																						} catch(e:Not_found) {
																							error(^("Invalid - main : ", ^(s_type_path(cl), " does not have static function main")), c.cl_pos);
																						};
																						};
																							var emain = type_type(ctx, cl, null_pos);
																							Some(mk(TCall(mk(TField(emain, fmode), ft, null_pos), []), r, null_pos));
																						};
																						};

																							public static function finalize(ctx) return {
																							flush_pass(ctx, PFinal, "final");
																						};

																							public static function generate(ctx) return {
																							var types = ref([]);
																							var states = Hashtbl.create(0);
																							function state(p) return {
																							try {
																							Hashtbl.find(states, p);
																						} catch(e:Not_found) {
																							NotYet;
																						};
																						};
																							var statics = ref(PMap.empty);
																							function loop(t) return {
																							var p = t_path(t);
																							switch (state(p)) {
																							case Done: [];
																							case Generating: ctx.com.warning(^("Warning : maybe loop in static generation of ", s_type_path(p)), t_infos(t).mt_pos);
																							case NotYet: Hashtbl.add(states, p, Generating);
																							var t = switch (t) {
																							case TClassDecl(c): walk_class(p, c);
																							t;
																							case TEnumDecl(_) | TTypeDecl(_) | TAbstractDecl(_): t;
																						};
																							Hashtbl.replace(states, p, Done);
																							types.val = ::(t, types.val);
																						};
																						};
																							function loop_class(p, c) return {
																							if (<>(c.cl_path, p)) {
																							loop(TClassDecl(c));
																						} else {
																							[];
																						};
																						};
																							function loop_enum(p, e) return {
																							if (<>(e.e_path, p)) {
																							loop(TEnumDecl(e));
																						} else {
																							[];
																						};
																						};
																							function loop_abstract(p, a) return {
																							if (<>(a.a_path, p)) {
																							loop(TAbstractDecl(a));
																						} else {
																							[];
																						};
																						};
																							function walk_static_field(p, c, cf) return {
																							switch (cf.cf_expr) {
																							case None: [];
																							case Some(e): if (PMap.mem((new Tuple(c.cl_path, cf.cf_name)), statics.val)) {
																							[];
																						} else {
																							statics.val = PMap.add((new Tuple(c.cl_path, cf.cf_name)), [], statics.val);
																							walk_expr(p, e);
																						};
																						};
																						};
																							function walk_expr(p, e) return {
																							switch (e.eexpr) {
																							case TTypeExpr(t): switch (t) {
																							case TClassDecl(c): loop_class(p, c);
																							case TEnumDecl(e): loop_enum(p, e);
																							case TAbstractDecl(a): loop_abstract(p, a);
																							case TTypeDecl(_): assert False;
																						};
																							case TNew(c, _, _): iter(walk_expr(p), e);
																							loop_class(p, c);
																							function loop(c) return {
																							if (PMap.mem((new Tuple(c.cl_path, "new")), statics.val)) {
																							[];
																						} else {
																							statics.val = PMap.add((new Tuple(c.cl_path, "new")), [], statics.val);
																							switch (c.cl_constructor) {
																							case Some({ cf_expr = Some(e) }): walk_expr(p, e);
																							case _: [];
																						};
																							switch (c.cl_super) {
																							case None: [];
																							case Some(csup, _): loop(csup);
																						};
																						};
																						};
																							loop(c);
																							case TField(e1, FStatic(c, cf)): walk_expr(p, e1);
																							walk_static_field(p, c, cf);
																							case _: iter(walk_expr(p), e);
																						};
																						};
																							function walk_class(p, c) return {
																							switch (c.cl_super) {
																							case None: [];
																							case Some(c, _): loop_class(p, c);
																						};
																							List.iter(function (c, _): loop_class(p, c), c.cl_implements);
																							switch (c.cl_init) {
																							case None: [];
																							case Some(e): walk_expr(p, e);
																						};
																							PMap.iter(function _: function f: switch (f.cf_expr) {
																							case None: [];
																							case Some(e): switch (e.eexpr) {
																							case TFunction(_): [];
																							case _: walk_expr(p, e);
																						};
																						}, c.cl_statics);
																						};
																							var sorted_modules = List.sort(function m1: function m2: compare(m1.m_path, m2.m_path), Hashtbl.fold(function _: function m: function acc: ::(m, acc), ctx.g.modules, []));
																							List.iter(function m: List.iter(loop, m.m_types), sorted_modules);
																							(new Tuple(get_main(ctx), List.rev(types.val), sorted_modules));
																						};

																							public static var macro_enable_cache = ref(False);

																							public static var macro_interp_cache = ref(None);

																							public static var delayed_macro_result = ref(function []: assert False : unit -> unit -> Interp.value);

																							public static function get_type_patch(ctx, t, sub) return {
																							function new_patch([]) return {
																						{ () with tp_type = None;
																							tp_remove = False;
																							tp_meta = [] };
																						};
																							var path = Ast.parse_path(t);
																							var Tuple(h, tp) = try {
																							Hashtbl.find(ctx.g.type_patches, path);
																						} catch(e:Not_found) {
																							var h = Hashtbl.create(0);
																							var tp = new_patch([]);
																							Hashtbl.add(ctx.g.type_patches, path, (new Tuple(h, tp)));
																							(new Tuple(h, tp));
																						};
																							switch (sub) {
																							case None: tp;
																							case Some(k): try {
																							Hashtbl.find(h, k);
																						} catch(e:Not_found) {
																							var tp = new_patch([]);
																							Hashtbl.add(h, k, tp);
																							tp;
																						};
																						};
																						};

																							public static function macro_timer(ctx, path) return {
																							Common.timer(if (Common.defined(ctx.com, Define.MacroTimes)) {
																							^("macro ", path);
																						} else {
																							"macro execution";
																						});
																						};

																							public static function typing_timer(ctx, f) return {
																							var t = Common.timer("typing");
																							var old = ctx.com.error;
																							var oldp = ctx.pass;
																							if (=(ctx.com.display, DMNone)) {
																							ctx.com.error = function e: function p: raise(Error(Custom(e), p));
																						} else {
																							[];
																						};
																							if (<(ctx.pass, PTypeField)) {
																							ctx.pass = PTypeField;
																						} else {
																							[];
																						};
																							function exit([]) return {
																							t([]);
																							ctx.com.error = old;
																							ctx.pass = oldp;
																						};
																							try {
																							var r = f([]);
																							exit([]);
																							r;
																						} catch(e:T) {
																							McOr(McArr(PaApp(PaApp(PaId(<...>),PaId(<...>)),PaId(IdLid(<...>))),ExNil,ExSeq(ExSem(ExApp(<...>,<...>),ExApp(<...>,<...>)))),McOr(McArr(PaApp(PaApp(<...>,<...>),PaId(<...>)),ExNil,ExSeq(ExSem(<...>,<...>))),McArr(PaId(IdLid(<...>)),ExNil,ExSeq(ExSem(<...>,<...>)))))			case Error(ekind, p): exit([]);
																							Interp.compiler_error(Typecore.error_msg(ekind), p);
																							case WithTypeError(l, p): exit([]);
																							Interp.compiler_error(Typecore.error_msg(Unify(l)), p);
																							case e: exit([]);
																							raise(e);
																						};
																						};

																							public static var load_macro_ref = ref(function _: function _: function _: function _: assert False) : ref(typer -> path -> string -> pos -> Tuple<typer * Tuple<list(Tuple<string * bool * t>) * t * tclass * Type.tclass_field> * list(Interp.value) -> option(Interp.value)>);

																							public static function make_macro_api(ctx, p) return {
																							function parse_expr_string(s, p, inl) return {
																							typing_timer(ctx, function []: parse_expr_string(ctx, s, p, inl));
																						};
																						{ () with Interp.pos = p;
																							Interp.get_com = function []: ctx.com;
																							Interp.get_type = function s: typing_timer(ctx, function []: var path = parse_path(s);
																							var tp = switch (List.rev(fst(path))) {
																							case ::(s, sl) if(&&(>(String.length(s), 0), switch (s0) {
																							case 'A' .. 'Z': True;
																							case _: False;
																						})): { () with tpackage = List.rev(sl);
																							tname = s;
																							tparams = [];
																							tsub = Some(snd(path)) };
																							case _: { () with tpackage = fst(path);
																							tname = snd(path);
																							tparams = [];
																							tsub = None };
																						};
																							try {
																							var m = Some(Typeload.load_instance(ctx, tp, p, True));
																							m;
																						} catch(e:Error(Module_not_found(_))(p2)) {
																							None;
																						});
																							Interp.get_module = function s: typing_timer(ctx, function []: var path = parse_path(s);
																							var m = List.map(type_of_module_type, Typeload.load_module(ctx, path, p).m_types);
																							m);
																							Interp.on_generate = function f: Common.add_filter(ctx.com, function []: var t = macro_timer(ctx, "onGenerate");
																							f(List.map(type_of_module_type, ctx.com.types));
																							t([]));
																							Interp.after_generate = function f: Common.add_final_filter(ctx.com, function []: var t = macro_timer(ctx, "afterGenerate");
																							f([]);
																							t([]));
																							Interp.on_type_not_found = function f: ctx.com.load_extern_type = @(ctx.com.load_extern_type, ::(function path: function p: switch (f(s_type_path(path))) {
																							case Interp.VNull: None;
																							case td: var Tuple(Tuple(pack, name), tdef, p) = Interp.decode_type_def(td);
																							Some(name, (new Tuple(pack, ::((new Tuple(tdef, p)), []))));
																						}, []));
																							Interp.parse_string = parse_expr_string;
																							Interp.type_expr = function e: typing_timer(ctx, function []: type_expr(ctx, e, Value));
																							Interp.type_macro_expr = function e: var e = typing_timer(ctx, function []: type_expr(ctx, e, Value));
																							function loop(e) return {
																							switch (e.eexpr) {
																							case TField(_, FStatic(c, { cf_kind = Method(_) } = cf)): ignore(load_macro_ref.val(ctx, c.cl_path, cf.cf_name, e.epos));
																							case _: Type.iter(loop, e);
																						};
																						};
																							loop(e);
																							e;
																							Interp.store_typed_expr = function te: var p = te.epos;
																							var id = get_next_stored_typed_expr_id([]);
																							ctx.com.stored_typed_exprs = PMap.add(id, te, ctx.com.stored_typed_exprs);
																							var eid = (new Tuple(EConst(Int(string_of_int(id))), p));
																							(new Tuple(EMeta((new Tuple(Meta.StoredTypedExpr, [], p)), eid), p));
																							Interp.get_display = function s: var is_displaying = <>(ctx.com.display, DMNone);
																							var old_resume = Parser.resume_display.val;
																							var old_error = ctx.on_error;
																							function restore([]) return {
																							if (!(is_displaying)) {
																							ctx.com.defines = PMap.remove(fst(Define.infos(Define.Display)), ctx.com.defines);
																							ctx.com.display = DMNone;
																						} else {
																							[];
																						};
																							Parser.resume_display.val = old_resume;
																							ctx.on_error = old_error;
																						};
																							if (!(is_displaying)) {
																							Common.define(ctx.com, Define.Display);
																							ctx.com.display = DMDefault;
																						} else {
																							[];
																						};
																							Parser.resume_display.val = { () with Ast.pfile = "macro";
																							Ast.pmin = 0;
																							Ast.pmax = 0 };
																							ctx.on_error = function ctx: function msg: function p: raise(Error(Custom(msg), p));
																							var str = try {
																							var e = parse_expr_string(s, Ast.null_pos, True);
																							var e = Optimizer.optimize_completion_expr(e);
																							ignore(type_expr(ctx, e, Value));
																							"NO COMPLETION";
																						} catch(e:T) {
																							McOr(McArr(PaApp(PaId(IdUid(<...>)),PaId(IdLid(<...>))),ExNil,ExLet(ReNil,BiEq(PaId(<...>),ExApp(<...>,<...>)),ExApp(ExApp(<...>,<...>),ExApp(<...>,<...>)))),McOr(McArr(PaApp(PaId(<...>),PaId(<...>)),ExNil,ExLet(ReNil,BiEq(<...>,<...>),ExApp(<...>,<...>))),McOr(McArr(PaApp(<...>,<...>),ExNil,ExMat(<...>,<...>)),McArr(PaApp(<...>,<...>),ExNil,ExApp(<...>,<...>)))))			case DisplayFields(fields): var pctx = print_context([]);
																							String.concat(", ", List.map(function (f, t, _, _): ^(f, ^(": ", s_type(pctx, t))), fields));
																							case DisplayTypes(tl): var pctx = print_context([]);
																							String.concat(", ", List.map(s_type(pctx), tl));
																							case Parser.TypePath(p, sub, _): switch (sub) {
																							case None: ^("path[", ^(String.concat(".", p), "]"));
																							case Some(c, _): ^("path[", ^(String.concat(".", p), ^(":", ^(c, "]"))));
																						};
																							case Typecore.Error(msg, p): ^("error[", ^(error_msg(msg), "]"));
																						};
																							restore([]);
																							str;
																							Interp.allow_package = function v: Common.allow_package(ctx.com, v);
																							Interp.type_patch = function t: function f: function s: function v: typing_timer(ctx, function []: var v = switch (v) {
																							case None: None;
																							case Some(s): switch (parse_string(ctx.com, ^("typedef T = ", s), null_pos, False)) {
																									case (_, ::((ETypedef({ d_data = ct }), _), [])): Some(ct);
																									case _: assert False;
																								};
																								};
																									var tp = get_type_patch(ctx, t, Some(f, s));
																									switch (v) {
																									case None: tp.tp_remove = True;
																									case Some(_): tp.tp_type = v;
																								});
																									Interp.meta_patch = function m: function t: function f: function s: var m = switch (parse_string(ctx.com, ^(m, " typedef T
																									= T"), null_pos, False)) {
																									  case (_, ::((ETypedef(t), _), [])): t.d_meta;
																									  case _: assert False;
																								  };
																									  var tp = get_type_patch(ctx, t, switch (f) {
																									  case None: None;
																									  case Some(f): Some(f, s);
																								  });
																									  tp.tp_meta = @(tp.tp_meta, m);
																									  Interp.set_js_generator = function gen: var js_ctx = Genjs.alloc_ctx(ctx.com);
																									  ctx.com.js_gen = Some(function []: var jsctx = Interp.enc_obj(::((new Tuple("outputFile", Interp.enc_string(ctx.com.file))), ::((new Tuple("types", Interp.enc_array(List.map(function t: Interp.encode_type(type_of_module_type(t)), ctx.com.types)))), ::((new Tuple("main", switch (ctx.com.main) {
																									  case None: Interp.VNull;
																									  case Some(e): Interp.encode_texpr(e);
																								  })), ::((new Tuple("generateValue", Interp.VFunction(Interp.Fun1(function v: var e = Interp.decode_texpr(v);
																									  var str = Genjs.gen_single_expr(js_ctx, e, False);
																									  Interp.enc_string(str))))), ::((new Tuple("isKeyword", Interp.VFunction(Interp.Fun1(function v: Interp.VBool(Hashtbl.mem(Genjs.kwds, Interp.dec_string(v))))))), ::((new Tuple("hasFeature", Interp.VFunction(Interp.Fun1(function v: Interp.VBool(Common.has_feature(ctx.com, Interp.dec_string(v))))))), ::((new Tuple("addFeature", Interp.VFunction(Interp.Fun1(function v: Common.add_feature(ctx.com, Interp.dec_string(v));
																									  Interp.VNull)))), ::((new Tuple("quoteString", Interp.VFunction(Interp.Fun1(function v: Interp.enc_string(^("\"",
																									^ (Ast.s_escape(Interp.dec_string(v)), "\""))))))), ::((new Tuple("buildMetaData",
																											Interp.VFunction(Interp.Fun1(function t:
																							switch (Codegen.build_metadata(ctx.com, Interp.decode_tdecl(t))) {
																						case None: Interp.VNull;
																							case Some(e): Interp.encode_texpr(e);
																							})))), ::((new Tuple("generateStatement", Interp.VFunction(Interp.Fun1(function v: var e = Interp.decode_texpr(v);
				var str = Genjs.gen_single_expr(js_ctx, e, True);
				Interp.enc_string(str))))), ::((new Tuple("setTypeAccessor",
												Interp.VFunction(Interp.Fun1(function callb: js_ctx.Genjs.type_accessor = function t: var v = Interp.encode_type(
														type_of_module_type(t));
														var ret = Interp.call(Interp.get_ctx([]), Interp.VNull, callb, ::(v, []), Nast.null_pos);
														Interp.dec_string(ret);
														Interp.VNull)))), ::((new Tuple("setCurrentClass", Interp.VFunction(Interp.Fun1(function c: Genjs.set_current_class(js_ctx,
				switch (Interp.decode_tdecl(c)) {
		case TClassDecl(c): c;
			case _: assert False;
			});
			Interp.VNull)))), [])))))))))))));
			var t = macro_timer(ctx, "jsGenerator");
			gen(jsctx);
			t([]));
			Interp.get_local_type = function []:
			switch (ctx.g.get_build_infos([])) {
		case Some(mt, tl, _): Some(switch (mt) {
			case TClassDecl(c): TInst(c, tl);
				case TEnumDecl(e): TEnum(e, tl);
				case TTypeDecl(t): TType(t, tl);
				case TAbstractDecl(a): TAbstract(a, tl);
				});
			case None: if ( == (ctx.curclass, null_class)) {
					None;
				} else {
					Some(TInst(ctx.curclass, []));
				};
			};
			Interp.get_expected_type = function []:
			switch (ctx.with_type_stack) {
		case ::(WithType(t) | WithTypeResume(t), _): Some(t);
			case _: None;
			};
			Interp.get_call_arguments = function []:
			switch (ctx.call_argument_stack) {
		case []: None;
			case ::(el, _): Some(el);
			};
			Interp.get_local_method = function []: ctx.curfield.cf_name;
			Interp.get_local_using = function []: ctx.m.module_using;
			Interp.get_local_imports = function []: ctx.m.module_imports;
			Interp.get_local_vars = function []: ctx.locals;
			Interp.get_build_fields = function []:
			switch (ctx.g.get_build_infos([])) {
		case None: Interp.VNull;
		case Some(_, _, fields): Interp.enc_array(List.map(Interp.encode_field, fields));
			};
			Interp.get_pattern_locals = function e: function t: get_pattern_locals_ref.val(ctx, e, t);
			Interp.define_type = function v: var Tuple(m, tdef, pos) = try {
				Interp.decode_type_def(v);
			} catch (e: Interp.Invalid_expr) {
				Interp.exc(Interp.VString("Invalid type definition"));
			};
			function add(ctx) return {
				var prev = try {
					Some(Hashtbl.find(ctx.g.modules, m));
				} catch (e: Not_found) {
					None;
				};
				var mnew = Typeload.type_module(ctx, m, ctx.m.curmod.m_extra.m_file, ::((new Tuple(tdef, pos)), []), pos);
				add_dependency(mnew, ctx.m.curmod);
				switch (prev) {
				case None: mnew.m_extra.m_kind = MFake;
				case Some(mold): Hashtbl.replace(ctx.g.modules, mnew.m_path, mold);
					mold.m_types = @(mold.m_types, mnew.m_types);
					mnew.m_extra.m_kind = MSub;
					add_dependency(mold, mnew);
				};
			};
			add(ctx);
			if (!(ctx.in_macro)) {
			switch ((new Tuple(tdef, ctx.g.macros))) {
				case (EClass(c), Some(_, mctx)) if (List.exists(function cff:
															|| (Meta.has(Meta.Macro, cff.cff_meta), List.mem(AMacro, cff.cff_access)), c.d_data)): add(mctx);
				case _: [];
				};
			} else {
				[];
			};
			Interp.define_module = function m: function types: function imports: function usings: var types = List.map(
			function v: var Tuple(_, tdef, pos) = try {
				Interp.decode_type_def(v);
			} catch (e: Interp.Invalid_expr) {
				Interp.exc(Interp.VString("Invalid type definition"));
			};
			(new Tuple(tdef, pos)), types);
			var pos = switch (types) {
		case []: Ast.null_pos;
			case ::((_, p), _): p;
			};
			var imports = List.map(function (il, ik): (new Tuple(EImport(il, ik), pos)), imports);
			var usings = List.map(function tp: (new Tuple(EUsing(tp), pos)), usings);
			var types = @(imports, @(usings, types));
			var m = Ast.parse_path(m);
			var prev = try {
				Some(Hashtbl.find(ctx.g.modules, m));
			} catch (e: Not_found) {
				None;
			};
			var mnew = Typeload.type_module(ctx, m, ctx.m.curmod.m_extra.m_file, types, pos);
			add_dependency(mnew, ctx.m.curmod);
			switch (prev) {
		case None: mnew.m_extra.m_kind = MFake;
		case Some(mold): Hashtbl.replace(ctx.g.modules, mnew.m_path, mold);
				mold.m_types = @(mold.m_types, mnew.m_types);
				mnew.m_extra.m_kind = MSub;
				add_dependency(mold, mnew);
			};
			Interp.module_dependency = function mpath: function file: function ismacro: var m = typing_timer(ctx,
									   function []: Typeload.load_module(ctx, parse_path(mpath), p));
			if (ismacro) {
			m.m_extra.m_macro_calls = ::(file, List.filter(file<>, m.m_extra.m_macro_calls));
			} else {
				add_dependency(m, create_fake_module(ctx, file));
			};
			Interp.current_module = function []: ctx.m.curmod;
			Interp.delayed_macro = function i: var mctx = switch (ctx.g.macros) {
		case None: assert False;
		case Some(_, mctx): mctx;
			};
			var f = try {
				DynArray.get(mctx.g.delayed_macros, i);
			} catch (e: _) {
				failwith("Delayed macro retrieve failure");
			};
			f([]);
			var ret = delayed_macro_result.val;
			delayed_macro_result.val = function []: assert False;
			ret;
			Interp.use_cache = function []: macro_enable_cache.val;
			Interp.format_string = function s: function p: format_string(ctx, s, p);
			Interp.cast_or_unify = function t: function e: function p: Codegen.AbstractCast.cast_or_unify_raise(ctx, t, e, p);
			Interp.add_global_metadata = function s1: function s2: function config: var meta = switch (parse_string(ctx.com, ^ (s2,
			" typedef T = T"), null_pos, False)) {
		case (_, ::((ETypedef(t), _), [])): t.d_meta;
			case _: assert False;
			};
			List.iter(function m: ctx.g.global_metadata = ::((new Tuple(ExtString.String.nsplit(s1, "."), m, config)),
					  ctx.g.global_metadata), meta)
		};
	};

	public static function init_macro_interp(ctx, mctx, mint) return {
		var p = Ast.null_pos;
		ignore(Typeload.load_module(mctx, (new Tuple(::("haxe", ::("macro", [])), "Expr")), p));
		ignore(Typeload.load_module(mctx, (new Tuple(::("haxe", ::("macro", [])), "Type")), p));
		flush_macro_context(mint, ctx);
		Interp.init(mint);
		if ( && (macro_enable_cache.val, !(Common.defined(mctx.com, Define.NoMacroCache)))) {
			macro_interp_cache.val = Some(mint);
		} else {
			[];
		};
	};

	public static function flush_macro_context(mint, ctx) return {
		var mctx = switch (ctx.g.macros) {
		case None: assert False;
		case Some(_, mctx): mctx;
		};
		finalize(mctx);
		var Tuple(_, types, modules) = generate(mctx);
		mctx.com.types = types;
		mctx.com.Common.modules = modules;
		var mint = if (!(Interp.can_reuse(mint, types))) {
			var com2 = mctx.com;
			var mint = Interp.create(com2, make_macro_api(ctx, Ast.null_pos));
			var macro = (new Tuple(function []: Interp.select(mint), mctx));
			ctx.g.macros = Some(macro);
			mctx.g.macros = Some(macro);
			init_macro_interp(ctx, mctx, mint);
			mint;
		} else {
			mint;
		};
		var expr_filters = ::(Codegen.AbstractCast.handle_abstract_casts(mctx), ::(Filters.captured_vars(mctx.com), ::(Filters.rename_local_vars(mctx), [])));
		function minimal_restore(t) return {
			switch (t) {
			case TClassDecl(c): var meta = c.cl_meta;
				var path = c.cl_path;
				c.cl_restore = function []: c.cl_meta = meta;
				c.cl_path = path;
			case _: [];
			};
		};
		var type_filters = ::(Filters.add_field_inits(mctx), ::(minimal_restore, ::(Filters.apply_native_paths(mctx), [])));
		function ready(t) return {
			Filters.apply_filters_once(mctx, expr_filters, t);
			List.iter(function f: f(t), type_filters);
		};
		try {
			Interp.add_types(mint, types, ready);
		} catch (e: Error(e)(p)) {
			raise(Fatal_error(error_msg(e), p));
		};
		Filters.next_compilation([]);
	};

	public static function create_macro_interp(ctx, mctx) return {
		var com2 = mctx.com;
		var Tuple(mint, init) = switch (macro_interp_cache.val) {
		case None: var mint = Interp.create(com2, make_macro_api(ctx, Ast.null_pos));
			(new Tuple(mint, function []: init_macro_interp(ctx, mctx, mint)));
		case Some(mint): Interp.do_reuse(mint, make_macro_api(ctx, Ast.null_pos));
			(new Tuple(mint, function []: []));
		};
		var on_error = com2.error;
		com2.error = function e: function p: Interp.set_error(Interp.get_ctx([]), True);
		macro_interp_cache.val = None;
		on_error(e, p);
		var macro = (new Tuple(function []: Interp.select(mint), mctx));
		ctx.g.macros = Some(macro);
		mctx.g.macros = Some(macro);
		init([]);
	};

	public static function get_macro_context(ctx, p) return {
		var api = make_macro_api(ctx, p);
		switch (ctx.g.macros) {
		case Some(select, ctx): select([]);
			(new Tuple(api, ctx));
		case None: var com2 = Common.clone(ctx.com);
			ctx.com.get_macros = function []: Some(com2);
			com2.package_rules = PMap.empty;
			com2.main_class = None;
			com2.display = DMNone;
			List.iter(function p: com2.defines = PMap.remove(platform_name(p), com2.defines), platforms);
			com2.defines_signature = None;
			com2.class_path = List.filter(function s: !(ExtString.String.exists(s, "/_std/")), com2.class_path);
			com2.class_path = @(List.map(function p: ^ (p, ^ ("neko", "/_std/")), com2.std_path), com2.class_path);
			var to_remove = List.map(function d: fst(Define.infos(d)), ::(Define.NoTraces, []));
			var to_remove = @(to_remove, List.map(function (_, d): ^ ("flash", d), Common.flash_versions));
			com2.defines = PMap.foldi(function k: function v: function acc:
			if (List.mem(k, to_remove)) {
			acc;
		} else {
			PMap.add(k, v, acc);
			}, com2.defines, PMap.empty);
			Common.define(com2, Define.Macro);
			Common.init_platform(com2, Neko);
			var mctx = ctx.g.do_create(com2);
			create_macro_interp(ctx, mctx);
			(new Tuple(api, mctx));
		};
	};

	public static function load_macro(ctx, cpath, f, p) return {
		var t = macro_timer(ctx, "typing [+init]");
		var Tuple(api, mctx) = get_macro_context(ctx, p);
		var mint = Interp.get_ctx([]);
		var Tuple(cpath, sub) = switch (List.rev(fst(cpath))) {
		case ::(name, pack) if (&&(>=(name0, 'A'), <=(name0, 'Z'))): (new Tuple((new Tuple(List.rev(pack), name)),
			Some(snd(cpath))));
		case _: (new Tuple(cpath, None));
		};
		var m = try {
			Hashtbl.find(ctx.g.types_module, cpath);
		} catch (e: Not_found) {
			cpath;
		};
		var mloaded = Typeload.load_module(mctx, m, p);
		mctx.m = { () with curmod = mloaded;
				   module_types = [];
				   module_using = [];
				   module_globals = PMap.empty;
				   wildcard_packages = [];
				   module_imports = []
				 };
		add_dependency(ctx.m.curmod, mloaded);
		var mt = Typeload.load_type_def(mctx, p, { () with tpackage = fst(cpath);
										tname = snd(cpath);
										tparams = [];
										tsub = sub
												 });
		var Tuple(cl, meth) = switch (mt) {
		case TClassDecl(c): finalize(mctx);
			(new Tuple(c, try {
				PMap.find(f, c.cl_statics);
			} catch (e: Not_found) {
				error( ^ ("Method ", ^ (f, ^ (" not found on class ", s_type_path(cpath)))), p);
			}));
		case _: error("Macro should be called on a class", p);
		};
		var meth = switch (follow(meth.cf_type)) {
		case TFun(args, ret): (new Tuple(args, ret, cl, meth));
		case _: error("Macro call should be a method", p);
		};
		if (!(ctx.in_macro)) {
			flush_macro_context(mint, ctx);
		} else {
			[];
		};
		t([]);
		function call(args) return {
			var t = macro_timer(ctx, ^ (s_type_path(cpath), ^ (".", f)));
			incr(stats.s_macros_called);
			var r = Interp.call_path(Interp.get_ctx([]), @(fst(cpath), ::(switch (sub) {
		case None: snd(cpath);
			case Some(s): s;
			}, [])), f, args, api);
			t([]);
			r;
		};
		(new Tuple(mctx, meth, call));
	};

	public static function type_macro(ctx, mode, cpath, f, ellist(Ast.expr), p) return {
		var Tuple(mctx, Tuple(margs, mret, mclass, mfield), call_macro) = load_macro(ctx, cpath, f, p);
		var mpos = mfield.cf_pos;
		var ctexpr = {
			() with tpackage = ::("haxe", ::("macro", []));
			tname = "Expr";
			tparams = [];
			tsub = None
		};
		var expr = Typeload.load_instance(mctx, ctexpr, p, False);
		switch (mode) {
		case MExpr: unify(mctx, mret, expr, mpos);
		case MBuild: var ctfields = { () with tpackage = [];
										  tname = "Array";
										  tparams = ::(TPType(CTPath({ () with tpackage = ::("haxe", ::("macro", []));
													tname = "Expr";
													tparams = [];
													tsub = Some("Field")
																	 })), []);
										  tsub = None
										};
			var tfields = Typeload.load_instance(mctx, ctfields, p, False);
			unify(mctx, mret, tfields, mpos);
		case MMacroType: var cttype = { () with tpackage = ::("haxe", ::("macro", []));
											tname = "Type";
											tparams = [];
											tsub = None
										  };
			var ttype = Typeload.load_instance(mctx, cttype, p, False);
			try {
				unify_raise(mctx, mret, ttype, mpos);
			} catch (e: Error(Unify(_))(_)) {
				var cttype = { () with tpackage = ::("haxe", ::("macro", []));
							   tname = "Expr";
							   tparams = [];
							   tsub = Some("ComplexType")
							 };
				var ttype = Typeload.load_instance(mctx, cttype, p, False);
				unify_raise(mctx, mret, ttype, mpos);
			};
		};
		var Tuple(el, el2) = switch (List.rev(margs)) {
		case ::((_, _, TInst({ cl_path = ([], Array) }, ::(e, []))), rest) if (try {
					Type.type_eq(EqStrict, e, expr);
						True;
					} catch (e: Unify_error(_)) {
						False;
					}): 	function loop(Tuple(acc1, acc2), el1, el2) return {
				switch ((new Tuple(el1, el2))) {
				case ([], []): (new Tuple(List.rev(acc1), List.rev(acc2)));
				case ([], ::(e2, [])): (new Tuple(List.rev(::((new Tuple(EArrayDecl([]), p)), acc1)), []));
				case ([], _): (new Tuple(List.rev(acc1), List.rev(acc2)));
				case (::(e1, l1), ::(e2, [])): loop((new Tuple(::((new Tuple(EArrayDecl([]), p)), acc1), ::(e1, []))), l1, []);
				case (::(e1, l1), []): loop((new Tuple(acc1, ::(e1, acc2))), l1, []);
				case (::(e1, l1), ::(e2, l2)): loop((new Tuple(::(e1, acc1), acc2)), l1, l2);
				};
			};
			loop((new Tuple([], [])), el, margs);
		case _: (new Tuple(el, []));
		};
		var todo = ref([]);
		var args = var eargs = List.map(function (n, o, t):
		try {
			unify_raise(mctx, t, expr, p);
			(new Tuple((new Tuple(n, o, t_dynamic)), MAExpr));
		} catch (e: Error(Unify(_))(_)) {
			switch (follow(t)) {
			case TFun(_): (new Tuple((new Tuple(n, o, t_dynamic)), MAFunction));
			case _: (new Tuple((new Tuple(n, o, t)), MAOther));
			};
		}, margs);
		var index = ref(-1);
		var constants = List.map(function e: var p = snd(e);
		var e = try {
			switch (Codegen.type_constant_value(ctx.com, e)) {
			case {
					eexpr = TConst(TString(_));
					epos = p
				} if (Lexer.is_fmt_string(p)): Lexer.remove_fmt_string(p);
				todo.val = ::(function []: Lexer.add_fmt_string(p), todo.val);
			case _: [];
			};
			e;
		} catch (e: Error(Custom(_))(_)) {
			(new Tuple(EBlock(::((new Tuple(EVars(::((new Tuple("__tmp", Some(CTPath(ctexpr)), Some(EConst(Ident("null")), p))), [])),
											p)), ::((new Tuple(EConst(Ident("__tmp")), p)), []))), p));
		};
		incr(index);
		(new Tuple(EArray((new Tuple(EArrayDecl(::(e, [])), p)), (new Tuple(EConst(Int(string_of_int(index.val))), p))), p)), el);
		var Tuple(elt, _) = unify_call_args(mctx, constants, List.map(fst, eargs), t_dynamic, p, False, False);
		List.iter(function f: f([]), todo.val);
		List.map2(function (_, mct): function e: var Tuple(e, et) = switch (e.eexpr) {
	case TArray({ eexpr = TArrayDecl(::(e, [])) }, { eexpr = TConst(TInt(index)) }): (new Tuple(List.nth(el,
					Int32.to_int(index)), e));
		case TConst(TNull): (new Tuple((new Tuple(EConst(Ident("null")), e.epos)), e));
		case _: assert False;
		};
		var ictx = Interp.get_ctx([]);
		switch (mct) {
	case MAExpr: Interp.encode_expr(e);
		case MAFunction: var e = ictx.Interp.curapi.Interp.type_macro_expr(e);
			switch (Interp.eval_expr(ictx, e)) {
			case Some(v): v;
			case None: Interp.VNull;
			};
		case MAOther: switch (Interp.eval_expr(ictx, et)) {
			case None: assert False;
			case Some(v): v;
			};
		}, eargs, elt);
		var args = switch (el2) {
		case []: args;
		case _: @(switch (List.rev(args)) {
		case ::(_, args): List.rev(args);
			case []: [];
			}, ::(Interp.enc_array(List.map(Interp.encode_expr, el2)), []));
		};
		function call([]) return {
			switch (call_macro(args)) {
			case None: None;
			case Some(v): try {
					Some(switch (mode) {
				case MExpr: Interp.decode_expr(v);
					case MBuild: var fields = switch (v) {
						case Interp.VNull: switch (ctx.g.get_build_infos([])) {
							case None: assert False;
							case Some(_, _, fields): fields;
							};
						case _: List.map(Interp.decode_field, Interp.dec_array(v));
						};
						(new Tuple(EVars(::((new Tuple("fields", Some(CTAnonymous(fields)), None)), [])), p));
					case MMacroType: var t = if ( = (v, Interp.VNull)) {
							mk_mono([]);
						} else {
							try {
								var ct = Interp.decode_ctype(v);
								Typeload.load_complex_type(ctx, p, ct);
							} catch (e: Interp.Invalid_expr) {
								Interp.decode_type(v);
							};
						};
						ctx.ret = t;
						(new Tuple(EBlock([]), p));
					});
				} catch (e: Interp.Invalid_expr) {
					if ( = (v, Interp.VNull)) {
						error("Unexpected null value returned from macro", p);
					} else {
						error("The macro didn't return a valid result", p);
					};
				};
			};
		};
		var e = if (ctx.in_macro) {
			var ctx = { (ctx) with locals = ctx.locals };
			var pos = DynArray.length(mctx.g.delayed_macros);
			DynArray.add(mctx.g.delayed_macros, function []: delayed_macro_result.val = function []: var mint = Interp.get_ctx([]);
			switch (call([])) {
		case None: function []: raise(Interp.Abort);
			case Some(e): Interp.eval(mint, Genneko.gen_expr(mint.Interp.gen, type_expr(ctx, e, Value)));
			});
			ctx.m.curmod.m_extra.m_time = -1.;
			var e = (new Tuple(EConst(Ident("__dollar__delay_call")), p));
			Some(EUntyped(ECall(e, ::((new Tuple(EConst(Int(string_of_int(pos))), p)), [])), p), p);
		} else {
			call([]);
		};
		e;
	};

	public static function call_macro(ctx, path, meth, args, p) return {
		var Tuple(mctx, Tuple(margs, _, mclass, mfield), call) = load_macro(ctx, path, meth, p);
		var Tuple(el, _) = unify_call_args(mctx, args, margs, t_dynamic, p, False, False);
		call(List.map(function e:
		try {
			Interp.make_const(e);
		} catch (e: Exit) {
			error("Parameter should be a constant", e.epos);
		}, el));
	};

	public static function call_init_macro(ctx, e) return {
		var p = {
			() with pfile = "--macro";
			pmin = 0;
			pmax = 0
		};
		var e = try {
			parse_expr_string(ctx, e, p, False);
		} catch (e: err) {
			display_error(ctx, ^ ("Could not parse `", ^ (e, "`")), p);
			raise(err);
		};
		switch (fst(e)) {
		case ECall(e, args): 	function loop(e) return {
				switch (fst(e)) {
				case EField(e, f): ::(f, loop(e));
				case EConst(Ident(i)): ::(i, []);
				case _: error("Invalid macro call", p);
				};
			};
			var Tuple(path, meth) = switch (loop(e)) {
			case ::(meth, []): (new Tuple((new Tuple(::("haxe", ::("macro", [])), "Compiler")), meth));
			case ::(meth, ::(cl, path)): (new Tuple((new Tuple(List.rev(path), cl)), meth));
			case _: error("Invalid macro call", p);
			};
			ignore(call_macro(ctx, path, meth, args, p));
		case _: error("Invalid macro call", p);
		};
	};

	public static function create(com) return {
		var ctx = {
			() with com = com;
			t = com.basic;
			g = {
				() with core_api = None;
				macros = None;
				modules = Hashtbl.create(0);
				types_module = Hashtbl.create(0);
				type_patches = Hashtbl.create(0);
				global_metadata = [];
				delayed = [];
				debug_delayed = [];
				delayed_macros = DynArray.create([]);
				doinline = !( || (Common.defined(com, Define.NoInline), <>(com.display, DMNone)));
				hook_generate = [];
				get_build_infos = function []: None;
				std = null_module;
				global_using = [];
				do_inherit = Codegen.on_inherit;
				do_create = create;
				do_macro = type_macro;
				do_load_module = Typeload.load_module;
				do_optimize = Optimizer.reduce_expression;
				do_build_instance = Codegen.build_instance
			};
			m = { () with curmod = null_module;
				  module_types = [];
				  module_using = [];
				  module_globals = PMap.empty;
				  wildcard_packages = [];
				  module_imports = []
				};
			meta = [];
			this_stack = [];
			with_type_stack = [];
			call_argument_stack = [];
			pass = PBuildModule;
			macro_depth = 0;
			untyped = False;
			curfun = FunStatic;
			in_loop = False;
			in_super_call = False;
			in_display = False;
			in_macro = Common.defined(com, Define.Macro);
			ret = mk_mono([]);
			locals = PMap.empty;
			type_params = [];
			curclass = null_class;
			curfield = null_field;
			tthis = mk_mono([]);
			opened = [];
			vthis = None;
			on_error = function ctx: function msg: function p: ctx.com.error(msg, p)
		};
		ctx.g.std = try {
			Typeload.load_module(ctx, (new Tuple([], "StdTypes")), null_pos);
		} catch (e: Error(Module_not_found([])(StdTypes))(_)) {
			error("Standard library not found", null_pos);
		};
		ctx.m.module_types = ctx.g.std.m_types;
		List.iter(function t:
		switch (t) {
	case TAbstractDecl(a): switch (snd(a.a_path)) {
			case Void: ctx.t.tvoid = TAbstract(a, []);
			case Float: ctx.t.tfloat = TAbstract(a, []);
			case Int: ctx.t.tint = TAbstract(a, []);
			case Bool: ctx.t.tbool = TAbstract(a, []);
			case _: [];
			};
		case TEnumDecl(e): [];
		case TClassDecl(c): [];
		case TTypeDecl(td): switch (snd(td.t_path)) {
			case Null: 	function mk_null(t) return {
					try {
						if (!(is_null(no_lazy = True, t))) {
							TType(td, ::(t, []));
						} else {
							t;
						};
					} catch (e: Exit) {
						var r = ref(function []: assert False);
						r.val = function []: var t = if (!(is_null(t))) {
							TType(td, ::(t, []));
						} else {
							t;
						};
						r.val = function []: t;
						t;
						TLazy(r);
					};
				};
				ctx.t.tnull = mk_null;
			case _: [];
			};
		}, ctx.g.std.m_types);
		var m = Typeload.load_module(ctx, (new Tuple([], "String")), null_pos);
		switch (m.m_types) {
		case ::(TClassDecl(c), []): ctx.t.tstring = TInst(c, []);
		case _: assert False;
		};
		var m = Typeload.load_module(ctx, (new Tuple([], "Array")), null_pos);
		try {
			List.iter(function t:
			switch (t) {
		case TClassDecl({ cl_path = ([], Array) } = c): ctx.t.tarray = function t: TInst(c, ::(t, []));
				raise(Exit);
			case _: [];
			}, m.m_types);
			assert False;
		} catch (e: Exit) {
			[];
		};
		var m = Typeload.load_module(ctx, (new Tuple(::("haxe", []), "EnumTools")), null_pos);
		switch (m.m_types) {
		case ::(TClassDecl(c1), ::(TClassDecl(c2), [])): ctx.g.global_using = ::(c1, ::(c2, ctx.g.global_using));
		case ::(TClassDecl(c1), []): var m = Typeload.load_module(ctx, (new Tuple(::("haxe", []), "EnumValueTools")), null_pos);
			switch (m.m_types) {
			case ::(TClassDecl(c2), []): ctx.g.global_using = ::(c1, ::(c2, ctx.g.global_using));
			case _: assert False;
			};
		case _: assert False;
		};
		ctx;
	};

	public static function __init__() {
		unify_min_ref.val = unify_min;
		make_call_ref.val = make_call;
		get_constructor_ref.val = get_constructor;
		cast_or_unify_ref.val = Codegen.AbstractCast.cast_or_unify_raise;
		type_module_type_ref.val = type_module_type;
		find_array_access_raise_ref.val = Codegen.AbstractCast.find_array_access_raise;
		build_call_ref.val = build_call;
		load_macro_ref.val = load_macro;
	}
}
;
