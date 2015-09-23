import Ast;
import Type;
import Common;
import Typecore;

class /*exception*/ Generic_Exception {

};

typedef Generic_context = {
	ctx : Typer,
	subst : List<Tuple<T, T>>,
	name : String,
	p : Pos,
	mg : Option<Module_def>
};

typedef Stack_context = {
	stack_var : String,
	stack_exc_var : String,
	stack_pos_var : String,
	stack_pos : Pos,
	stack_expr : Texpr,
	stack_pop : Texpr,
	stack_save_pos : Texpr,
	stack_restore : List<Texpr>,
	stack_push : Tclass -> String -> Texpr,
	stack_return : Texpr -> Texpr
};

class Codegen {
	public static function field(e, name, t, p) return {
		mk(TField(e, try {
			quick_field(e.etype, name);
		} catch (e: Not_found) {
			assert False;
		}), t, p);
	};

	public static function fcall(e, name, el, ret, p) return {
		var ft = tfun(List.map(function e: e.etype, el), ret);
		mk(TCall(field(e, name, ft, p), el), ret, p);
	};

	public static function mk_parent(e) return {
		mk(TParenthesis(e), e.etype, e.epos);
	};

	public static function string(com, str, p) return {
		mk(TConst(TString(str)), com.basic.tstring, p);
	};

	public static function binop(op, a, b, t, p) return {
		mk(TBinop(op, a, b), t, p);
	};

	public static function index(com, e, index, t, p) return {
		mk(TArray(e, mk(TConst(TInt(Int32.of_int(index))), com.basic.tint, p)), t, p);
	};

	public static function maybe_cast(e, t) return {
		try {
			type_eq(EqDoNotFollowNull, e.etype, t);
			e;
		} catch (e: Unify_error(_)) {
			mk(TCast(e, None), t, e.epos);
		};
	};

	public static function type_constant(com, c, p) return {
		var t = com.basic;
		switch (c) {
		case Int(s): if ( && ( > (String.length(s), 10), = (String.sub(s, 0, 2), "0x"))) {
				error("Invalid hexadecimal integer", p);
			} else {
				[];
			};
			try {
				mk(TConst(TInt(Int32.of_string(s))), t.tint, p);
			} catch (e: _) {
				mk(TConst(TFloat(s)), t.tfloat, p);
			};
		case Float(f): mk(TConst(TFloat(f)), t.tfloat, p);
		case String(s): mk(TConst(TString(s)), t.tstring, p);
		case Ident(true): mk(TConst(TBool(True)), t.tbool, p);
		case Ident(false): mk(TConst(TBool(False)), t.tbool, p);
		case Ident(null): mk(TConst(TNull), t.tnull(mk_mono([])), p);
		case Ident(t): error( ^ ("Invalid constant :  ", t), p);
		case Regexp(_): error("Invalid constant", p);
		};
	};

	public static function type_constant_value(com, Tuple(e, p)) return {
		switch (e) {
		case EConst(c): type_constant(com, c, p);
		case EParenthesis(e): type_constant_value(com, e);
		case EObjectDecl(el): mk(TObjectDecl(List.map(function (n, e): (new Tuple(n, type_constant_value(com, e))), el)), TAnon({
				() with a_fields = PMap.empty;
				a_status = ref(Closed)
			}), p);
		case EArrayDecl(el): mk(TArrayDecl(List.map(type_constant_value(com), el)), com.basic.tarray(t_dynamic), p);
		case _: error("Constant value expected", p);
		};
	};

	public static function has_properties(c) return {
		|| (List.exists(function f:
		switch (f.cf_kind) {
	case Var({ v_read = AccCall }): True;
		case Var({ v_write = AccCall }): True;
		case _ if (Meta.has(Meta.Accessor, f.cf_meta)): True;
		case _: False;
		}, c.cl_ordered_fields), switch (c.cl_super) {
	case Some(c, _): has_properties(c);
		case _: False;
		});
	};

	public static function get_properties(fields) return {
		List.fold_left(function acc: function f:
		if (Meta.has(Meta.Accessor, f.cf_meta)) {
		::((new Tuple(f.cf_name, f.cf_name)), acc);
		} else {
			var acc = switch (f.cf_kind) {
			case Var({ v_read = AccCall }): ::((new Tuple(^("get_", f.cf_name), ^("get_", f.cf_name))), acc);
			case _: acc;
			};
			switch (f.cf_kind) {
			case Var({ v_write = AccCall }): ::((new Tuple(^("set_", f.cf_name), ^("set_", f.cf_name))), acc);
			case _: acc;
			};
		}, [], fields);
	};

	public static function add_property_field(com, c) return {
		var p = c.cl_pos;
		var props = get_properties(@(c.cl_ordered_statics, c.cl_ordered_fields));
		switch (props) {
		case []: [];
		case _: var Tuple(fields, values) = List.fold_left(function (fields, values): function (n, v): var cf = mk_field(n,
			com.basic.tstring, p);
			(new Tuple(PMap.add(n, cf, fields), ::((new Tuple(n, string(com, v, p))), values))), (new Tuple(PMap.empty, [])), props);
			var t = mk_anon(fields);
			var e = mk(TObjectDecl(values), t, p);
			var cf = mk_field("__properties__", t, p);
			cf.cf_expr = Some(e);
			c.cl_statics = PMap.add(cf.cf_name, cf, c.cl_statics);
			c.cl_ordered_statics = ::(cf, c.cl_ordered_statics);
		};
	};

	public static function is_removable_field(ctx, f) return {
		|| (Meta.has(Meta.Extern, f.cf_meta), || (Meta.has(Meta.Generic, f.cf_meta), switch (f.cf_kind) {
	case Var({ v_read = AccRequire(s, _) }): True;
		case Method(MethMacro): !(ctx.in_macro);
		case _: False;
		}));
	};

	public static function escape_res_name(name, allow_dirs) return {
		ExtString.String.replace_chars(function chr:
		if ( || ( && ( >= (chr, 'a'), <= (chr, 'z')), || ( && ( >= (chr, 'A'), <= (chr, 'Z')), || ( && ( >= (chr, '0'), <= (chr, '9')), || ( = (chr, '_'), = (chr, '.')))))) {
		Char.escaped(chr);
		} else {
			if ( && ( = (chr, '/'), allow_dirs)) {
				"/";
			} else {
				^ ("-x", string_of_int(Char.code(chr)));
			};
		}, name);
	};

	public static function extend_remoting(ctx, c, t, p, async, prot) return {
		if (<>(c.cl_super, None)) {
			error("Cannot extend several classes", p);
		} else {
			[];
		};
		var rules = ctx.com.package_rules;
		ctx.com.package_rules = PMap.foldi(function key: function r: function acc:
		switch (r) {
	case Forbidden: acc;
	case _: PMap.add(key, r, acc);
		}, rules, PMap.empty);
		var path = (new Tuple(t.tpackage, t.tname));
		var new_name = ^ (if (async) {
		"Async_";
	} else {
		"Remoting_";
	}, t.tname);
		var t = try {
			Typeload.load_type_def(ctx, p, {
				() with tpackage = fst(path);
				tname = new_name;
				tparams = [];
				tsub = None
			});
		} catch (e: Error(Module_not_found(_))(p2)) {
			Common.log(ctx.com, ^ ("Building proxy for ", s_type_path(path)));
			var Tuple(file, decls) = try {
				Typeload.parse_module(ctx, path, p);
			} catch (e: T) {
				McOr(McArr(PaId(IdUid(Not_found)), ExNil, ExSeq(ExSem(ExAss(<...>, <...>), ExApp(<...>, <...>)))), McArr(PaId(IdLid(e)),
					ExNil, ExSeq(ExSem(ExAss(<...>, <...>), ExApp(<...>, <...>)))))				case Not_found: ctx.com.package_rules = rules;
				error( ^ ("Could not load proxy module ", ^ (s_type_path(path), if ( = (fst(path), [])) {
				", [try using absolute path]";
			} else {
				"";
			})), p);
			case e: ctx.com.package_rules = rules;
				raise(e);
			};
			ctx.com.package_rules = rules;
			var base_fields = ::({ () with cff_name = "__cnx";
								   cff_pos = p;
								   cff_doc = None;
								   cff_meta = [];
								   cff_access = [];
								   cff_kind = FVar(Some(CTPath({ () with tpackage = ::("haxe", ::("remoting", []));
			tname = if (async) {
			"AsyncConnection";
		} else {
			"Connection";
		};
		tparams = [];
				  tsub = None
														   })), None)
							 }, ::({ () with cff_name = "new";
									 cff_pos = p;
									 cff_doc = None;
									 cff_meta = [];
									 cff_access = ::(APublic, []);
									 cff_kind = FFun({ () with f_args = ::((new Tuple("c", False, None, None)), []);
												f_type = None;
												f_expr = Some(EBinop(OpAssign, (new Tuple(EConst(Ident("__cnx")), p)), (new Tuple(EConst(Ident("c")), p))), p);
												f_params = []
													 })
								   }, []));
			var tvoid = CTPath({ () with tpackage = [];
								 tname = "Void";
								 tparams = [];
								 tsub = None
							   });
			function build_field(is_public, acc, f) return {
				if ( = (f.cff_name, "new")) {
					acc;
				} else {
					switch (f.cff_kind) {
					case FFun(fd) if (&&(||(is_public, List.mem(APublic, f.cff_access)), !(List.mem(AStatic, f.cff_access)))):
						if (List.exists(function (_, _, t, _): = (t, None), fd.f_args)) {
							error( ^ ("Field ", ^ (f.cff_name, " type is not complete and cannot be used by RemotingProxy")), p);
						} else {
							[];
						};
						var eargs = ::((new Tuple(EArrayDecl(List.map(function (a, _, _, _): (new Tuple(EConst(Ident(a)), p)), fd.f_args)),
												  p)), []);
						var ftype = switch (fd.f_type) {
						case Some(CTPath({ tpackage = []; tname = Void })): None;
						case _: fd.f_type;
						};
						var Tuple(fargs, eargs) = if (async) {
							switch (ftype) {
							case Some(tret): (new Tuple(@(fd.f_args, ::((new Tuple("__callb", True, Some(CTFunction(::(tret, []), tvoid)),
															  None)), [])), @(eargs, ::((new Tuple(EConst(Ident("__callb")), p)), []))));
							case _: (new Tuple(fd.f_args, @(eargs, ::((new Tuple(EConst(Ident("null")), p)), []))));
							};
						} else {
							(new Tuple(fd.f_args, eargs));
						};
						var id = (new Tuple(EConst(String(f.cff_name)), p));
						var id = if (prot) {
							id;
						} else {
							(new Tuple(ECall((new Tuple(EConst(Ident("__unprotect__")), p)), ::(id, [])), p));
						};
						var expr = (new Tuple(ECall((new Tuple(EField((new Tuple(ECall((new Tuple(EField((new Tuple(EConst(Ident("__cnx")), p)),
															   "resolve"), p)), ::(id, [])), p)), "call"), p)), eargs), p));
						var expr = if ( || (async, = (ftype, None))) {
							expr;
						} else {
							(new Tuple(EReturn(Some(expr)), p));
						};
						var fd = { () with f_params = fd.f_params;
								   f_args = fargs;
						f_type = if (async) {
						None;
					} else {
						ftype;
					};
					f_expr = Some(EBlock(::(expr, [])), p)
							 };
					::({ () with cff_name = f.cff_name;
						 cff_pos = p;
						 cff_doc = None;
						 cff_meta = [];
						 cff_access = ::(APublic, []);
						 cff_kind = FFun(fd)
					   }, acc);
					case _: acc;
					};
				};
			};
			var decls = List.map(function d:
			switch (d) {
		case (EClass(c), p) if (=(c.d_name, t.tname)): var is_public =
					|| (List.mem(HExtern, c.d_flags), List.mem(HInterface, c.d_flags));
				var fields = List.rev(List.fold_left(build_field(is_public), base_fields, c.d_data));
				(new Tuple(EClass({ (c) with d_flags = [];
									d_name = new_name;
									d_data = fields
								  }), p));
			case _: d;
			}, decls);
			var m = Typeload.type_module(ctx, (new Tuple(t.tpackage, new_name)), file, decls, p);
			add_dependency(ctx.m.curmod, m);
			try {
				List.find(function tdecl: = (snd(t_path(tdecl)), new_name), m.m_types);
			} catch (e: Not_found) {
				error( ^ ("Module ", ^ (s_type_path(path), ^ (" does not define type ", t.tname))), p);
			};
		};
		switch (t) {
		case TClassDecl(c2) if (=(c2.cl_params, [])): ignore(c2.cl_build([]));
			c.cl_super = Some(c2, []);
		case _: error("Remoting proxy must be a class without parameters", p);
		};
	};

	public static function make_generic(ctx, ps, pt, p) return {
		function loop(l1, l2) return {
			switch ((new Tuple(l1, l2))) {
			case ([], []): [];
			case (::((x, TLazy(f)), l1), _): loop(::((new Tuple(x, f.val([]))), l1), l2);
			case (::((_, t1), l1), ::(t2, l2)): ::((new Tuple(t1, t2)), loop(l1, l2));
			case _: assert False;
			};
		};
		var name = String.concat("_", List.map2(function (s, _): function t: 	function s_type_path_underscore(Tuple(p, s)) return {
			switch (p) {
			case []: s;
			case _: ^ (String.concat("_", p), ^ ("_", s));
			};
		};
		function loop(top, t) return {
			switch (follow(t)) {
			case TInst(c, tl): ^ (s_type_path_underscore(c.cl_path), loop_tl(tl));
			case TEnum(en, tl): ^ (s_type_path_underscore(en.e_path), loop_tl(tl));
			case TAbstract(a, tl): ^ (s_type_path_underscore(a.a_path), loop_tl(tl));
			case _ if (!(top)): "_";
			case TMono(_): raise(Generic_Exception( ^ ("Could not determine type for parameter ", s), p));
			case TDynamic(_): "Dynamic";
			case t: raise(Generic_Exception( ^ ("Type parameter must be a class or enum instance [found ", ^ (s_type(print_context([]),
				t), "]")), p));
			};
		};
		function loop_tl(tl) return {
			switch (tl) {
			case []: "";
			case tl: ^ ("_", String.concat("_", List.map(loop(False), tl)));
			};
		};
		loop(True, t), ps, pt));
		{
			() with ctx = ctx;
			subst = loop(ps, pt);
			name = name;
			p = p;
			mg = None
		};
	};

	public static function generic_substitute_type(gctx, t) return {
		switch (t) {
		case TInst({ cl_kind = KGeneric } = c2, tl2): var Tuple(_, _, f) = gctx.ctx.g.do_build_instance(gctx.ctx, TClassDecl(c2),
			gctx.p);
			var t = f(List.map(generic_substitute_type(gctx), tl2));
			switch ((new Tuple(follow(t), gctx.mg))) {
			case (TInst(c, _), Some(m)): add_dependency(m, c.cl_module);
			case _: [];
			};
			t;
		case _: try {
				generic_substitute_type(gctx, List.assq(t, gctx.subst));
			} catch (e: Not_found) {
				Type.map(generic_substitute_type(gctx), t);
			};
		};
	};

	public static function generic_substitute_expr(gctx, e) return {
		var vars = Hashtbl.create(0);
		function build_var(v) return {
			try {
				Hashtbl.find(vars, v.v_id);
			} catch (e: Not_found) {
				var v2 = alloc_var(v.v_name, generic_substitute_type(gctx, v.v_type));
				v2.v_meta = v.v_meta;
				Hashtbl.add(vars, v.v_id, v2);
				v2;
			};
		};
		function build_expr(e) return {
			switch (e.eexpr) {
			case TField(e1, FInstance({ cl_kind = KGeneric } = c, tl, cf)): var Tuple(_, _, f) = gctx.ctx.g.do_build_instance(gctx.ctx,
				TClassDecl(c), gctx.p);
				var t = f(List.map(generic_substitute_type(gctx), tl));
				build_expr({ (e) with eexpr = TField(e1, quick_field(t, cf.cf_name)) });
			case _: map_expr_type(build_expr, generic_substitute_type(gctx), build_var, e);
			};
		};
		build_expr(e);
	};

	public static function has_ctor_constraint(c) return {
		switch (c.cl_kind) {
		case KTypeParameter(tl): List.exists(function t: switch (follow(t)) {
		case TAnon(a) if (PMap.mem("new", a.a_fields)): True;
			case _: False;
			}, tl);
		case _: False;
		};
	};

	public static var get_short_name = var i = ref(-1);
	function []: incr(i);
	Printf.sprintf("Hx___short___hx_type_%i", i.val);

	public static function build_generic(ctx, c, p, tl) return {
		var pack = fst(c.cl_path);
		var recurse = ref(False);
		function check_recursive(t) return {
			switch (follow(t)) {
			case TInst(c2, tl): switch (c2.cl_kind) {
				case KTypeParameter(tl): if ( && (!(Typeload.is_generic_parameter(ctx, c2)), has_ctor_constraint(c2))) {
						error("Type parameters with a constructor cannot be used non-generically", p);
					} else {
						[];
					};
					recurse.val = True;
				case _: [];
				};
				List.iter(check_recursive, tl);
			case _: [];
			};
		};
		List.iter(check_recursive, tl);
		if (recurse.val) {
			TInst(c, tl);
		} else {
			var gctx = make_generic(ctx, c.cl_params, tl, p);
			var name = ^ (snd(c.cl_path), ^ ("_", gctx.name));
			try {
				Typeload.load_instance(ctx, {
					() with tpackage = pack;
					tname = name;
					tparams = [];
					tsub = None
				}, p, False);
			} catch (e: Error(Module_not_found(path))(_)) {
				var m = try {
					Hashtbl.find(ctx.g.modules, Hashtbl.find(ctx.g.types_module, c.cl_path));
				} catch (e: Not_found) {
					assert False;
				};
				var ctx = { (ctx) with m = { (ctx.m) with module_types = @(m.m_types, ctx.m.module_types) } };
				ignore(c.cl_build([]));
				var mg = { () with m_id = alloc_mid([]);
						   m_path = (new Tuple(pack, name));
						   m_types = [];
						   m_extra = module_extra(s_type_path((new Tuple(pack, name))), m.m_extra.m_sign, 0., MFake)
						 };
				gctx.mg = Some(mg);
				var cg = mk_class(mg, (new Tuple(pack, name)), c.cl_pos);
				mg.m_types = ::(TClassDecl(cg), []);
				Hashtbl.add(ctx.g.modules, mg.m_path, mg);
				add_dependency(mg, m);
				add_dependency(ctx.m.curmod, mg);
				var dep_stack = ref([]);
				function loop(t) return {
					if (!(List.memq(t, dep_stack.val))) {
						dep_stack.val = ::(t, dep_stack.val);
						switch (t) {
						case TInst(c, tl): add_dep(c.cl_module, tl);
						case TEnum(e, tl): add_dep(e.e_module, tl);
						case TType(t, tl): add_dep(t.t_module, tl);
						case TAbstract(a, tl): add_dep(a.a_module, tl);
						case TMono(r): switch (r.val) {
							case None: [];
							case Some(t): loop(t);
							};
						case TLazy(f): loop(f.val([]));
						case TDynamic(t2): if ( == (t, t2)) {
								[];
							} else {
								loop(t2);
							};
						case TAnon(a): PMap.iter(function _: function f: loop(f.cf_type), a.a_fields);
						case TFun(args, ret): List.iter(function (_, _, t): loop(t), args);
							loop(ret);
						};
					} else {
						[];
					};
				};
				function add_dep(m, tl) return {
					add_dependency(mg, m);
					List.iter(loop, tl);
				};
				List.iter(loop, tl);
				function build_field(cf_old) return {
					var cf_new = { (cf_old) with cf_pos = cf_old.cf_pos };
					function f([]) return {
						var t = generic_substitute_type(gctx, cf_old.cf_type);
						ignore(follow(t));
						try {
							switch (cf_old.cf_expr) {
							case None: switch (cf_old.cf_kind) {
								case Method(_) if (&&(!(c.cl_interface), !(c.cl_extern))): display_error(ctx,
									Printf.sprintf("Field %s has no expression [possible typing order issue]", cf_new.cf_name), cf_new.cf_pos);
									display_error(ctx, Printf.sprintf("While building %s", s_type_path(cg.cl_path)), p);
								case _: [];
								};
							case Some(e): cf_new.cf_expr = Some(generic_substitute_expr(gctx, e));
							};
						} catch (e: Unify_error(l)) {
							error(error_msg(Unify(l)), cf_new.cf_pos);
						};
						t;
					};
					var r = exc_protect(ctx, function r: var t = mk_mono([]);
										r.val = function []: t;
										unify_raise(ctx, f([]), t, p);
										t, "build_generic");
					delay(ctx, PForce, function []: ignore(r.val([])));
					cf_new.cf_type = TLazy(r);
					cf_new;
				};
				if ( || (<>(c.cl_init, None), <>(c.cl_dynamic, None))) {
					error("This class can't be generic", p);
				} else {
					[];
				};
				List.iter(function cf:
				switch (cf.cf_kind) {
			case Method(MethMacro) if (!(ctx.in_macro)): [];
				case _: error("A generic class can't have static fields", cf.cf_pos);
				}, c.cl_ordered_statics);
				cg.cl_super = switch (c.cl_super) {
				case None: None;
				case Some(cs, pl): 	function find_class(subst) return {
						function loop(subst) return {
							switch (subst) {
							case ::((TInst(c, []), t), subst) if (==(c, cs)): t;
							case ::(_, subst): loop(subst);
							case []: raise(Not_found);
							};
						};
						try {
							if (<>(pl, [])) {
								raise(Not_found);
							} else {
								[];
							};
							var t = loop(subst);
							switch ((new Tuple(follow(t), c.cl_constructor))) {
							case (TInst(cs, _), None): ignore(cs.cl_build([]));
								switch (cs.cl_constructor) {
								case None: error( ^ ("Cannot use ", ^ (s_type_path(cs.cl_path),
																		   " as type parameter because it is extended and has no constructor")), p);
								case _: [];
								};
							case (_, Some(cf)): error("Generics extending type parameters cannot have constructors", cf.cf_pos);
							case _: [];
							};
							t;
						} catch (e: Not_found) {
							apply_params(c.cl_params, tl, TInst(cs, pl));
						};
					};
					var ts = follow(find_class(gctx.subst));
					var Tuple(cs, pl) = Typeload.check_extends(ctx, c, ts, p);
					switch (cs.cl_kind) {
					case KGeneric: switch (build_generic(ctx, cs, p, pl)) {
						case TInst(cs, pl): Some(cs, pl);
						case _: assert False;
						};
					case _: Some(cs, pl);
					};
				};
				Typeload.add_constructor(ctx, cg, False, p);
				cg.cl_kind = KGenericInstance(c, tl);
				cg.cl_meta = ::((new Tuple(Meta.NoDoc, [], p)), cg.cl_meta);
				cg.cl_interface = c.cl_interface;
				cg.cl_constructor = switch ((new Tuple(cg.cl_constructor, c.cl_constructor, c.cl_super))) {
				case (_, Some(cf), _): Some(build_field(cf));
				case (Some(ctor), _, _): Some(ctor);
				case (None, None, None): None;
				case _: error("Please define a constructor for this class in order to use it as generic", c.cl_pos);
				};
				cg.cl_implements = List.map(function (i, tl):
				switch (follow(generic_substitute_type(gctx, TInst(i, List.map(generic_substitute_type(gctx), tl))))) {
			case TInst(i, tl): (new Tuple(i, tl));
				case _: assert False;
				}, c.cl_implements);
				cg.cl_ordered_fields = List.map(function f: var f = build_field(f);
												cg.cl_fields = PMap.add(f.cf_name, f, cg.cl_fields);
												f, c.cl_ordered_fields);
				if ( > (String.length(snd(cg.cl_path)), 254)) {
					var n = get_short_name([]);
					cg.cl_meta = ::((new Tuple(Meta.Native, ::((new Tuple(EConst(String(n)), p)), []), p)), cg.cl_meta);
				} else {
					[];
				};
				TInst(cg, []);
			};
		};
	};

	public static function extend_xml_proxy(ctx, c, t, file, p) return {
		var t = Typeload.load_complex_type(ctx, p, t);
		var file = try {
			Common.find_file(ctx.com, file);
		} catch (e: Not_found) {
			file;
		};
		add_dependency(c.cl_module, create_fake_module(ctx, file));
		var used = ref(PMap.empty);
		function print_results([]) return {
			PMap.iter(function id: function used:
			if (!(used)) {
			ctx.com.warning( ^ (id, " is not used"), p);
			} else {
				[];
			}, used.val);
		};
		var check_used = Common.defined(ctx.com, Define.CheckXmlProxy);
		if (check_used) {
			ctx.g.hook_generate = ::(print_results, ctx.g.hook_generate);
		} else {
			[];
		};
		try {
			function loop(match) return switch (match) {
			case Xml.Element(_, attrs, childs): try {
					var id = List.assoc("id", attrs);
					if (PMap.mem(id, c.cl_fields)) {
						error( ^ ("Duplicate id ", id), p);
					} else {
						[];
					};
					var t = if (!(check_used)) {
						t;
					} else {
						used.val = PMap.add(id, False, used.val);
						function ft([]) return {
							used.val = PMap.add(id, True, used.val);
							t;
						};
						TLazy(ref(ft));
					};
					var f = { () with cf_name = id;
							  cf_type = t;
							  cf_public = True;
							  cf_pos = p;
							  cf_doc = None;
							  cf_meta = no_meta;
							  cf_kind = Var({ () with v_read = AccResolve;
											  v_write = AccNo
											});
							  cf_params = [];
							  cf_expr = None;
							  cf_overloads = []
							};
					c.cl_fields = PMap.add(id, f, c.cl_fields);
				} catch (e: Not_found) {
					[];
				};
				List.iter(loop, childs);
			case Xml.PCData(_): [];
			};
			loop(Xml.parse_file(file));
		} catch (e: T) {
			McOr(McArr(PaApp(PaId(IdAcc(<...>, <...>)), PaId(IdLid(<...>))), ExNil, ExApp(ExApp(ExId(<...>), ExApp(<...>, <...>)),
					   ExId(IdLid(<...>)))), McArr(PaApp(PaId(IdAcc(<...>, <...>)), PaId(IdLid(<...>))), ExNil, ExApp(ExApp(ExId(<...>),
											   ExApp(<...>, <...>)), ExId(IdLid(<...>)))))			case Xml.Error(e): error( ^ ("XML error ", Xml.error(e)), p);
		case Xml.File_not_found(f): error( ^ ("XML File not found : ", f), p);
		};
	};

	public static function build_metadata(com, t) return {
		var api = com.basic;
		var Tuple(p, meta, fields, statics) = switch (t) {
		case TClassDecl(c): var fields = List.map(function f: (new Tuple(f.cf_name, f.cf_meta)), @(c.cl_ordered_fields,
			switch (c.cl_constructor) {
		case None: [];
			case Some(f): ::({ (f) with cf_name = "_" }, []);
			}));
			var statics = List.map(function f: (new Tuple(f.cf_name, f.cf_meta)), c.cl_ordered_statics);
			(new Tuple(c.cl_pos, ::((new Tuple("", c.cl_meta)), []), fields, statics));
		case TEnumDecl(e): (new Tuple(e.e_pos, ::((new Tuple("", e.e_meta)), []), List.map(function n: (new Tuple(n, PMap.find(n,
										  e.e_constrs).ef_meta)), e.e_names), []));
		case TTypeDecl(t): (new Tuple(t.t_pos, ::((new Tuple("", t.t_meta)), []), switch (follow(t.t_type)) {
		case TAnon(a): PMap.fold(function f: function acc: ::((new Tuple(f.cf_name, f.cf_meta)), acc), a.a_fields, []);
			case _: [];
			}, []));
		case TAbstractDecl(a): (new Tuple(a.a_pos, ::((new Tuple("", a.a_meta)), []), [], []));
		};
		function filter(l) return {
			var l = List.map(function (n, ml): (new Tuple(n, ExtList.List.filter_map(function (m, el, p):
			switch (m) {
		case Meta.Custom(s) if (&&(>(String.length(s), 0), <>(s0, ':'))): Some(s, el, p);
			case _: None;
			}, ml))), l);
			List.filter(function (_, ml): <>(ml, []), l);
		};
		var Tuple(meta, fields, statics) = (new Tuple(filter(meta), filter(fields), filter(statics)));
		function make_meta_field(ml) return {
			var h = Hashtbl.create(0);
			mk(TObjectDecl(List.map(function (f, el, p):
			if (Hashtbl.mem(h, f)) {
			error( ^ ("Duplicate metadata '", ^ (f, "'")), p);
			} else {
				[];
			};
			Hashtbl.add(h, f, []);
			(new Tuple(f, mk(switch (el) {
		case []: TConst(TNull);
			case _: TArrayDecl(List.map(type_constant_value(com), el));
			}, api.tarray(t_dynamic), p))), ml)), t_dynamic, p);
		};
		function make_meta(l) return {
			mk(TObjectDecl(List.map(function (f, ml): (new Tuple(f, make_meta_field(ml))), l)), t_dynamic, p);
		};
		if ( && ( = (meta, []), && ( = (fields, []), = (statics, [])))) {
			None;
		} else {
			var meta_obj = [];
			var meta_obj = if ( = (fields, [])) {
				meta_obj;
			} else {
				::((new Tuple("fields", make_meta(fields))), meta_obj);
			};
			var meta_obj = if ( = (statics, [])) {
				meta_obj;
			} else {
				::((new Tuple("statics", make_meta(statics))), meta_obj);
			};
			var meta_obj = try {
				::((new Tuple("obj", make_meta_field(List.assoc("", meta)))), meta_obj);
			} catch (e: Not_found) {
				meta_obj;
			};
			Some(mk(TObjectDecl(meta_obj), t_dynamic, p));
		};
	};

	public static function get_macro_path(ctx, e, args, p) return {
		function loop(e) return {
			switch (fst(e)) {
			case EField(e, f): ::(f, loop(e));
			case EConst(Ident(i)): ::(i, []);
			case _: error("Invalid macro call", p);
			};
		};
		var path = switch (e) {
		case (EConst(Ident(i)), _): var path = try {
				if (!(PMap.mem(i, ctx.curclass.cl_statics))) {
					raise(Not_found);
				} else {
					[];
				};
				ctx.curclass.cl_path;
			} catch (e: Not_found) {
				try {
					t_infos(fst(PMap.find(i, ctx.m.module_globals))).mt_path;
				} catch (e: Not_found) {
					error("Invalid macro call", p);
				};
			};
			::(i, ::(snd(path), fst(path)));
		case _: loop(e);
		};
		switch (path) {
		case ::(meth, ::(cl, path)): (new Tuple((new Tuple(List.rev(path), cl)), meth, args));
		case _: error("Invalid macro call", p);
		};
	};

	public static function build_macro_type(ctx, pl, p) return {
		var Tuple(path, field, args) = switch (pl) {
		case ::(TInst({ cl_kind = KExpr(ECall(e, args), _) }, _), []) | ::(TInst({ cl_kind = KExpr(EArrayDecl(::((ECall(e, args), _), [])), _) }, _), [])
			: get_macro_path(ctx, e, args, p);
		case _: error("MacroType requires a single expression call parameter", p);
		};
		var old = ctx.ret;
		var t = switch (ctx.g.do_macro(ctx, MMacroType, path, field, args, p)) {
		case None: mk_mono([]);
		case Some(_): ctx.ret;
		};
		ctx.ret = old;
		t;
	};

	public static function build_macro_build(ctx, c, pl, cfl, p) return {
		var Tuple(path, field, args) = switch (Meta.get(Meta.GenericBuild, c.cl_meta)) {
		case (_, ::((ECall(e, args), _), []), _): get_macro_path(ctx, e, args, p);
		case _: error("genericBuild requires a single expression call parameter", p);
		};
		var old = (new Tuple(ctx.ret, ctx.g.get_build_infos));
		ctx.g.get_build_infos = function []: Some(TClassDecl(c), pl, cfl);
		var t = switch (ctx.g.do_macro(ctx, MMacroType, path, field, args, p)) {
		case None: mk_mono([]);
		case Some(_): ctx.ret;
		};
		ctx.ret = fst(old);
		ctx.g.get_build_infos = snd(old);
		t;
	};

	public static function build_instance(ctx, mtype, p) return {
		switch (mtype) {
		case TClassDecl(c): if ( > (ctx.pass, PBuildClass)) {
				ignore(c.cl_build([]));
			} else {
				[];
			};
			function build(f, s) return {
				var r = exc_protect(ctx, function r: var t = mk_mono([]);
				r.val = function []: t;
				unify_raise(ctx, f([]), t, p);
				t, s);
				delay(ctx, PForce, function []: ignore(r.val([])));
				TLazy(r);
			};
			function ft(pl) return {
				switch (c.cl_kind) {
				case KGeneric: build(function []: build_generic(ctx, c, p, pl), "build_generic");
				case KMacroType: build(function []: build_macro_type(ctx, pl, p), "macro_type");
				case KGenericBuild(cfl): build(function []: build_macro_build(ctx, c, pl, cfl, p), "generic_build");
				case _: TInst(c, pl);
				};
			};
			(new Tuple(c.cl_params, c.cl_path, ft));
		case TEnumDecl(e): (new Tuple(e.e_params, e.e_path, function t: TEnum(e, t)));
		case TTypeDecl(t): (new Tuple(t.t_params, t.t_path, function tl: TType(t, tl)));
		case TAbstractDecl(a): (new Tuple(a.a_params, a.a_path, function tl: TAbstract(a, tl)));
		};
	};

	public static function on_inherit(ctx, c, p, h) return {
		switch (h) {
		case HExtends({ tpackage = ::(haxe, ::(remoting, [])); tname = Proxy; tparams = ::(TPType(CTPath(t)), []) }):
			extend_remoting(ctx, c, t, p, False, True);
			False;
		case HExtends({ tpackage = ::(haxe, ::(remoting, [])); tname = AsyncProxy; tparams = ::(TPType(CTPath(t)), []) }):
			extend_remoting(ctx, c, t, p, True, True);
			False;
		case HExtends({ tpackage = ::(haxe, ::(xml, [])); tname = Proxy; tparams = ::(TPExpr(EConst(String(file)), p), ::(TPType(t), [])) })
				: extend_xml_proxy(ctx, c, t, file, p);
			True;
		case _: True;
		};
	};

	public static function push_this(ctx, e) return {
		switch (e.eexpr) {
		case TConst(TInt(_) | TFloat(_) | TString(_) | TBool(_) = ct): (new Tuple((new Tuple(EConst(tconst_to_const(ct)), e.epos)),
			function []: []));
		case _: ctx.this_stack = ::(e, ctx.this_stack);
			var er = (new Tuple(EMeta((new Tuple(Meta.This, [], e.epos)), (new Tuple(EConst(Ident("this")), e.epos))), e.epos));
			(new Tuple(er, function []: ctx.this_stack = List.tl(ctx.this_stack)));
		};
	};

	public static function detect_usage(com) return {
		var usage = ref([]);
		List.iter(function t:
		switch (t) {
	case TClassDecl(c): 	function check_constructor(c, p) return {
				try {
					var Tuple(_, cf) = get_constructor(function cf: cf.cf_type, c);
					if (Meta.has(Meta.Usage, cf.cf_meta)) {
						usage.val = ::(p, usage.val);
					} else {
						[];
					};
				} catch (e: Not_found) {
					[];
				};
			};
			function expr(e) return {
				switch (e.eexpr) {
				case TField(_, FEnum(_, ef)) if (Meta.has(Meta.Usage, ef.ef_meta)): var p = { (e.epos) with pmin = -(e.epos.pmax, String.length(ef.ef_name)) };
					usage.val = ::(p, usage.val);
					Type.iter(expr, e);
				case TField(_, FAnon(cf) | FInstance(_, _, cf) | FStatic(_, cf) | FClosure(_, cf)) if (Meta.has(Meta.Usage, cf.cf_meta)):
					var p = { (e.epos) with pmin = -(e.epos.pmax, String.length(cf.cf_name)) };
					usage.val = ::(p, usage.val);
					Type.iter(expr, e);
				case TLocal(v) if (Meta.has(Meta.Usage, v.v_meta)): usage.val = ::(e.epos, usage.val);
				case TVar(v, _) if (&&(=(com.display, DMPosition), Meta.has(Meta.Usage, v.v_meta))): raise(Typecore.DisplayPosition(::
							(e.epos, [])));
				case TFunction(tf) if (&&(=(com.display, DMPosition), List.exists(function (v, _): Meta.has(Meta.Usage, v.v_meta),
												  tf.tf_args))): raise(Typecore.DisplayPosition(::(e.epos, [])));
				case TTypeExpr(mt) if (Meta.has(Meta.Usage, t_infos(mt).mt_meta)): usage.val = ::(e.epos, usage.val);
				case TNew(c, _, _): check_constructor(c, e.epos);
					Type.iter(expr, e);
				case TCall({ eexpr = TConst(TSuper) }, _):
					switch (c.cl_super) {
					case Some(c, _): check_constructor(c, e.epos);
					case _: [];
					};
				case _: Type.iter(expr, e);
				};
			};
			function field(cf) return {
				ignore(follow(cf.cf_type));
				switch (cf.cf_expr) {
				case None: [];
				case Some(e): expr(e);
				};
			};
			switch (c.cl_constructor) {
			case None: [];
			case Some(cf): field(cf);
			};
			switch (c.cl_init) {
			case None: [];
			case Some(e): expr(e);
			};
			List.iter(field, c.cl_ordered_statics);
			List.iter(field, c.cl_ordered_fields);
		case _: [];
		}, com.types);
		var usage = List.sort(function p1: function p2: var c = compare(p1.pfile, p2.pfile);
		if (<>(c, 0)) {
		c;
	} else {
		compare(p1.pmin, p2.pmin);
		}, usage.val);
		raise(Typecore.DisplayPosition(usage));
	};

	public static function update_cache_dependencies(com) return {
		function check_t(m, t) return {
			switch (t) {
			case TInst(c, tl): add_dependency(m, c.cl_module);
				List.iter(check_t(m), tl);
			case TEnum(en, tl): add_dependency(m, en.e_module);
				List.iter(check_t(m), tl);
			case TType(t, tl): add_dependency(m, t.t_module);
				List.iter(check_t(m), tl);
			case TAbstract(a, tl): add_dependency(m, a.a_module);
				List.iter(check_t(m), tl);
			case TFun(targs, tret): List.iter(function (_, _, t): check_t(m, t), targs);
				check_t(m, tret);
			case TAnon(an): PMap.iter(function _: function cf: check_field(m, cf), an.a_fields);
			case TMono(r): switch (r.val) {
				case Some(t): check_t(m, t);
				case _: [];
				};
			case TLazy(f): check_t(m, f.val([]));
			case TDynamic(t): if ( == (t, t_dynamic)) {
					[];
				} else {
					check_t(m, t);
				};
			};
		};
		function check_field(m, cf) return {
			check_t(m, cf.cf_type);
		};
		List.iter(function t:
		switch (t) {
	case TClassDecl(c): List.iter(check_field(c.cl_module), c.cl_ordered_statics);
			List.iter(check_field(c.cl_module), c.cl_ordered_fields);
			switch (c.cl_constructor) {
			case None: [];
			case Some(cf): check_field(c.cl_module, cf);
			};
		case _: [];
		}, com.types);
	};

	public static function stack_context_init(com, stack_var, exc_var, pos_var, tmp_var, use_add, p) return {
		var t = com.basic;
		var st = t.tarray(t.tstring);
		var stack_var = alloc_var(stack_var, st);
		var exc_var = alloc_var(exc_var, st);
		var pos_var = alloc_var(pos_var, t.tint);
		var stack_e = mk(TLocal(stack_var), st, p);
		var exc_e = mk(TLocal(exc_var), st, p);
		var stack_pop = fcall(stack_e, "pop", [], t.tstring, p);
		function stack_push(c, m) return {
			fcall(stack_e, "push", ::(if (use_add) {
			binop(OpAdd, string(com, ^ (s_type_path(c.cl_path), "::"), p), string(com, m, p), t.tstring, p);
			} else {
				string(com, ^ (s_type_path(c.cl_path), ^ ("::", m)), p);
			}, []), t.tvoid, p);
		};
		function stack_return(e) return {
			var tmp = alloc_var(tmp_var, e.etype);
			mk(TBlock(::(mk(TVar(tmp, Some(e)), t.tvoid, e.epos), ::(stack_pop, ::(mk(TReturn(Some(mk(TLocal(tmp), e.etype, e.epos))), e.etype, e.epos), [])))), e.etype, e.epos);
		};
		{
			() with stack_var = stack_var.v_name;
			stack_exc_var = exc_var.v_name;
			stack_pos_var = pos_var.v_name;
			stack_pos = p;
			stack_expr = stack_e;
			stack_pop = stack_pop;
			stack_save_pos = mk(TVar(pos_var, Some(field(stack_e, "length", t.tint, p))), t.tvoid, p);
			stack_push = stack_push;
			stack_return = stack_return;
			stack_restore = ::(binop(OpAssign, exc_e, mk(TArrayDecl([]), st, p), st, p), ::(mk(TWhile(mk_parent(binop(OpGte, field(stack_e, "length", t.tint, p), mk(TLocal(pos_var), t.tint, p), t.tbool, p)), fcall(exc_e, "unshift", ::(fcall(stack_e, "pop", [], t.tstring, p), []), t.tvoid, p), NormalWhile), t.tvoid, p), ::(fcall(stack_e, "push", ::(index(com, exc_e, 0, t.tstring, p), []), t.tvoid, p), [])))
		};
	};

	public static function stack_init(com, use_add) return {
		stack_context_init(com, "$s", "$e", "$spos", "$tmp", use_add, null_pos);
	};

	public static function stack_block_loop(ctx, e) return {
		switch (e.eexpr) {
		case TFunction(_): e;
		case TReturn(None) | TReturn(Some({ eexpr = TConst(_) })) | TReturn(Some({ eexpr = TLocal(_) })): mk(TBlock(::
			(ctx.stack_pop, ::(e, []))), e.etype, e.epos);
		case TReturn(Some(e)): ctx.stack_return(stack_block_loop(ctx, e));
		case TTry(v, cases): var v = stack_block_loop(ctx, v);
			var cases = List.map(function (v, e): var e = stack_block_loop(ctx, e);
			var e = switch (mk_block(e).eexpr) {
		case TBlock(l): mk(TBlock(@(ctx.stack_restore, l)), e.etype, e.epos);
			case _: assert False;
			};
			(new Tuple(v, e)), cases);
			mk(TTry(v, cases), e.etype, e.epos);
		case _: map_expr(stack_block_loop(ctx), e);
		};
	};

	public static function stack_block(ctx, c, m, e) return {
		switch (mk_block(e).eexpr) {
		case TBlock(l): mk(TBlock(@(::(ctx.stack_push(c, m), ::(ctx.stack_save_pos, List.map(stack_block_loop(ctx), l))),
			::(ctx.stack_pop, []))), e.etype, e.epos);
		case _: assert False;
		};
	};

	public static function find_field(com, c, f) return {
		try {
			switch (c.cl_super) {
			case None: raise(Not_found);
			case Some({ cl_path = (::(cpp, []), FastIterator) }, _): raise(Not_found);
			case Some(c, _): find_field(com, c, f);
			};
		} catch (e: Not_found) {
			try {
				if ( = (com.platform, Cpp)) {
					raise(Not_found);
				} else {
					[];
				};
				function loop(match) return switch (match) {
				case []: raise(Not_found);
				case ::((c, _), l): try {
						find_field(com, c, f);
					} catch (e: Not_found) {
						loop(l);
					};
				};
				loop(c.cl_implements);
			} catch (e: Not_found) {
				var f = PMap.find(f.cf_name, c.cl_fields);
				switch (f.cf_kind) {
				case Var({ v_read = AccRequire(_) }): raise(Not_found);
				case _: [];
				};
				f;
			};
		};
	};

	public static function fix_override(com, c, f, fd) return {
		var f2 = try {
			Some(find_field(com, c, f));
		} catch (e: Not_found) {
			None;
		};
		switch ((new Tuple(f2, fd))) {
		case (Some(f2), Some(fd)): var Tuple(targs, tret) = switch (follow(f2.cf_type)) {
			case TFun(args, ret): (new Tuple(args, ret));
			case _: assert False;
			};
			var changed_args = ref([]);
			var prefix = "_tmp_";
			var nargs = List.map2(function (v, ct) = cur: function (_, _, t2):
			try {
				type_eq(EqStrict, monomorphs(c.cl_params, monomorphs(f.cf_params, v.v_type)), t2);
				switch (follow(v.v_type)) {
				case TInst({ cl_kind = KTypeParameter(::(tc, [])) } = cp, _) if (=(com.platform, Flash)):
					if (List.mem_assoc(snd(cp.cl_path), c.cl_params)) {
						raise(Unify_error([]));
					} else {
						[];
					};
				case _: [];
				};
				cur;
			} catch (e: Unify_error(_)) {
				var v2 = alloc_var( ^ (prefix, v.v_name), t2);
				changed_args.val = ::((new Tuple(v, v2)), changed_args.val);
				(new Tuple(v2, ct));
			}, fd.tf_args, targs);
			var fd2 = { () with tf_args = nargs;
						tf_type = tret;
			tf_expr = switch (List.rev(changed_args.val)) {
		case []: fd.tf_expr;
			case args: var e = fd.tf_expr;
				var el = switch (e.eexpr) {
				case TBlock(el): el;
				case _: ::(e, []);
				};
				var p = switch (el) {
				case []: e.epos;
				case ::(e, _): e.epos;
				};
				var el_v = List.map(function (v, v2): mk(TVar(v, Some(mk(TCast(mk(TLocal(v2), v2.v_type, p), None), v.v_type, p))),
									com.basic.tvoid, p), args);
				{
					(e) with eexpr = TBlock(@(el_v, el))
				};
			}
					  };
			if ( && (Common.defined(com, Define.As3), f.cf_public)) {
				f2.cf_public = True;
			} else {
				[];
			};
			var targs = List.map(function (v, c): (new Tuple(v.v_name, Option.is_some(c), v.v_type)), nargs);
			var fde = switch (f.cf_expr) {
			case None: assert False;
			case Some(e): e;
			};
			f.cf_expr = Some({ (fde) with eexpr = TFunction(fd2) });
			f.cf_type = TFun(targs, tret);
		case (Some(f2), None) if (c.cl_interface): var Tuple(targs, tret) = switch (follow(f2.cf_type)) {
			case TFun(args, ret): (new Tuple(args, ret));
			case _: assert False;
			};
			f.cf_type = TFun(targs, tret);
		case _: [];
		};
	};

	public static function fix_overrides(com, t) return {
		switch (t) {
		case TClassDecl(c): if (c.cl_interface) {
				c.cl_ordered_fields = List.filter(function f:
				try {
					if ( == (find_field(com, c, f), f)) {
						raise(Not_found);
					} else {
						[];
					};
					c.cl_fields = PMap.remove(f.cf_name, c.cl_fields);
					False;
				} catch (e: Not_found) {
					True;
				}, c.cl_ordered_fields);
			} else {
				[];
			};
			List.iter(function f:
			switch ((new Tuple(f.cf_expr, f.cf_kind))) {
		case (Some({ eexpr = TFunction(fd) }), Method(MethNormal | MethInline)): fix_override(com, c, f, Some(fd));
			case (None, Method(MethNormal | MethInline)) if (c.cl_interface): fix_override(com, c, f, None);
			case _: [];
			}, c.cl_ordered_fields);
		case _: [];
		};
	};

	public static function fix_abstract_inheritance(com, t) return {
		switch (t) {
		case TClassDecl(c) if (c.cl_interface): c.cl_ordered_fields = List.filter(function f: var b = try {
				== (find_field(com, c, f), f);
			} catch (e: Not_found) {
				False;
			};
			if (!(b)) {
			c.cl_fields = PMap.remove(f.cf_name, c.cl_fields);
			} else {
				[];
			};
			b, c.cl_ordered_fields);
		case _: [];
		};
	};

	public static function is_volatile(t) return {
		switch (t) {
		case TMono(r): switch (r.val) {
			case Some(t): is_volatile(t);
			case _: False;
			};
		case TLazy(f): is_volatile(f.val([]));
		case TType(t, tl): switch (t.t_path) {
				_: is_volatile(apply_params(t.t_params, tl, t.t_type));
			};
		case _: False;
		};
	};

	public static function set_default(ctx, a, c, p) return {
		var t = a.v_type;
		var ve = mk(TLocal(a), t, p);
		var cond = TBinop(OpEq, ve, mk(TConst(TNull), t, p));
		mk(TIf(mk_parent(mk(cond, ctx.basic.tbool, p)), mk(TBinop(OpAssign, ve, mk(TConst(c), t, p)), t, p), None), ctx.basic.tvoid, p);
	};

	public static function bytes_serialize(data) return {
		var b64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
		var tbl = Array.init(String.length(b64), function i: String.get(b64, i));
		Base64.str_encode(tbl = , data);
	};

	public static function constructor_side_effects(e) return {
		switch (e.eexpr) {
		case TBinop(op, _, _) if (<>(op, OpAssign)): True;
		case TField(_, FEnum(_)): False;
		case TUnop(_) | TArray(_) | TField(_) | TEnumParameter(_) | TCall(_) | TNew(_) | TFor(_) | TWhile(_) | TSwitch(_) | TReturn(_) | TThrow(_)
				: True;
		case TBinop(_) | TTry(_) | TIf(_) | TBlock(_) | TVar(_) | TFunction(_) | TArrayDecl(_) | TObjectDecl(_) | TParenthesis(_) | TTypeExpr(_) | TLocal(_) | TMeta(_) | TConst(_) | TContinue | TBreak | TCast(_)
				: try {
				Type.iter(function e:
				if (constructor_side_effects(e)) {
				raise(Exit);
				} else {
					[];
				}, e);
				False;
			} catch (e: Exit) {
				True;
			};
		};
	};

	public static function make_valid_filename(s) return {
		var r = Str.regexp("[^A-Za-z0-9_\\-\\.,]");
		Str.global_substitute(r, function s: "_", s);
	};

	public static function create_dumpfile(acc) return {
	case []: assert False;
	case ::(d, []): var d = make_valid_filename(d);
		var ch = open_out( ^ (String.concat("/", List.rev(::(d, acc))), ".dump"));
		var buf = Buffer.create(0);
		(new Tuple(buf, function []: output_string(ch, Buffer.contents(buf));
		close_out(ch)));
	case ::(d, l): var dir = String.concat("/", List.rev(::(d, acc)));
		if (!(Sys.file_exists(dir))) {
			Unix.mkdir(dir, 0o755);
		} else {
			[];
		};
		create_dumpfile(::(d, acc), l);
	};

	public static function dump_types(com) return {
		var s_type = s_type(Type.print_context([]));
		function params(match) return switch (match) {
		case []: "";
		case l: Printf.sprintf("<%s>", String.concat(",", List.map(function (n, t): ^ (n, ^ (" : ", s_type(t))), l)));
		};
		var s_expr = switch (Common.defined_value_safe(com, Define.Dump)) {
		case pretty: Type.s_expr_pretty("\t");
		case legacy: Type.s_expr;
		case _: Type.s_expr_ast(!(Common.defined(com, Define.DumpIgnoreVarIds)), "\t");
		};
		List.iter(function mt: var path = Type.t_path(mt);
				  var Tuple(buf, close) = create_dumpfile([], @(::("dump", ::(Common.platform_name(com.platform), fst(path))), ::(snd(path), [])));
		function print(fmt) return {
			Printf.kprintf(function s: Buffer.add_string(buf, s), fmt);
		};
		switch (mt) {
	case Type.TClassDecl(c): 	function print_field(stat, f) return {
				print("\t%s%s%s%s", if (stat) {
				"static ";
			} else {
				"";
			}, if (f.cf_public) {
				"public ";
			} else {
				"";
			}, f.cf_name, params(f.cf_params));
				print("[%s] : %s", s_kind(f.cf_kind), s_type(f.cf_type));
				switch (f.cf_expr) {
				case None: [];
				case Some(e): print("\n\n\t = %s", s_expr(s_type, e));
				};
				print("\n\n");
				List.iter(function f: print_field(stat, f), f.cf_overloads);
			};
			print("%s%s%s %s%s", if (c.cl_private) {
			"private ";
		} else {
			"";
		}, if (c.cl_extern) {
			"extern ";
		} else {
			"";
		}, if (c.cl_interface) {
			"interface";
		} else {
			"class";
		}, s_type_path(path), params(c.cl_params));
			switch (c.cl_super) {
			case None: [];
			case Some(c, pl): print(" extends %s", s_type(TInst(c, pl)));
			};
			List.iter(function (c, pl): print(" implements %s", s_type(TInst(c, pl))), c.cl_implements);
			switch (c.cl_dynamic) {
			case None: [];
			case Some(t): print(" implements Dynamic<%s>", s_type(t));
			};
			switch (c.cl_array_access) {
			case None: [];
			case Some(t): print(" implements ArrayAccess<%s>", s_type(t));
			};
			print("{\n");
			switch (c.cl_constructor) {
			case None: [];
			case Some(f): print_field(False, f);
			};
			List.iter(print_field(False), c.cl_ordered_fields);
			List.iter(print_field(True), c.cl_ordered_statics);
			switch (c.cl_init) {
			case None: [];
			case Some(e): print("\n\n\t__init__ = ");
				print("%s", s_expr(s_type, e));
				print("}\n");
			};
			print("}");
		case Type.TEnumDecl(e): print("%s%senum %s%s {\n", if (e.e_private) {
			"private ";
		} else {
			"";
		}, if (e.e_extern) {
			"extern ";
		} else {
			"";
		}, s_type_path(path), params(e.e_params));
			List.iter(function n: var f = PMap.find(n, e.e_constrs);
					  print("\t%s : %s;\n", f.ef_name, s_type(f.ef_type)), e.e_names);
			print("}");
		case Type.TTypeDecl(t): print("%stype %s%s = %s", if (t.t_private) {
			"private ";
		} else {
			"";
		}, s_type_path(path), params(t.t_params), s_type(t.t_type));
		case Type.TAbstractDecl(a): print("%sabstract %s%s {}", if (a.a_private) {
			"private ";
		} else {
			"";
		}, s_type_path(path), params(a.a_params));
		};
		close([]), com.types);
	};

	public static function dump_dependencies(com) return {
		var Tuple(buf, close) = create_dumpfile([], ::("dump", ::(Common.platform_name(com.platform), ::(".dependencies", []))));
		function print(fmt) return {
			Printf.kprintf(function s: Buffer.add_string(buf, s), fmt);
		};
		var dep = Hashtbl.create(0);
		List.iter(function m: print("%s:\n", m.m_extra.m_file);
				  PMap.iter(function _: function m2: print("\t%s\n", m2.m_extra.m_file);
		var l = try {
			Hashtbl.find(dep, m2.m_extra.m_file);
		} catch (e: Not_found) {
			[];
		};
		Hashtbl.replace(dep, m2.m_extra.m_file, ::(m, l)), m.m_extra.m_deps), com.Common.modules);
		close([]);
		var Tuple(buf, close) = create_dumpfile([], ::("dump", ::(Common.platform_name(com.platform), ::(".dependants", []))));
		function print(fmt) return {
			Printf.kprintf(function s: Buffer.add_string(buf, s), fmt);
		};
		Hashtbl.iter(function n: function ml: print("%s:\n", n);
					 List.iter(function m: print("\t%s\n", m.m_extra.m_file), ml), dep);
		close([]);
	};

	public static function default_cast( ? : (vtmp = "$t"), com, e, texpr, t, p) return {
		var api = com.basic;
		function mk_texpr(match) return switch (match) {
		case TClassDecl(c): TAnon({
				() with a_fields = PMap.empty;
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
		var vtmp = alloc_var(vtmp, e.etype);
		var var = mk(TVar(vtmp, Some(e)), api.tvoid, p);
		var vexpr = mk(TLocal(vtmp), e.etype, p);
		var texpr = mk(TTypeExpr(texpr), mk_texpr(texpr), p);
		var std = try {
			List.find(function t: = (t_path(t), (new Tuple([], "Std"))), com.types);
		} catch (e: Not_found) {
			assert False;
		};
		var fis = try {
			var c = switch (std) {
			case TClassDecl(c): c;
			case _: assert False;
			};
			FStatic(c, PMap.find("is", c.cl_statics));
		} catch (e: Not_found) {
			assert False;
		};
		var std = mk(TTypeExpr(std), mk_texpr(std), p);
		var is = mk(TField(std, fis), tfun(::(t_dynamic, ::(t_dynamic, [])), api.tbool), p);
		var is = mk(TCall(is, ::(vexpr, ::(texpr, []))), api.tbool, p);
		var exc = mk(TThrow(mk(TConst(TString("Class cast error")), api.tstring, p)), t, p);
		var check = mk(TIf(mk_parent(is), mk(TCast(vexpr, None), t, p), Some(exc)), t, p);
		mk(TBlock(::(var, ::(check, ::(vexpr, [])))), t, p);
	};

	public static function interpolate_code(com, code, tl, f_string, f_expr, p) return {
		var exprs = Array.of_list(tl);
		var i = ref(0);
		function err(msg) return {
			var pos = { (p) with pmin = +(p.pmin, i.val) };
			com.error(msg, pos);
		};
		var regex = Str.regexp("[{}]");
		function loop(m) return {
			switch (m) {
			case []: [];
			case ::(Str.Text(txt), tl): i.val = +(i.val, String.length(txt));
				f_string(txt);
				loop(tl);
			case ::(Str.Delim(a), ::(Str.Delim(b), tl)) if (=(a, b)): i.val = +(i.val, 2);
				f_string(a);
				loop(tl);
			case ::(Str.Delim({), ::(Str.Text(n), ::(Str.Delim(}), tl))):
				try {
					var expr = Array.get(exprs, int_of_string(n));
					f_expr(expr);
					i.val = +(+(i.val, 2), String.length(n));
					loop(tl);
				} catch (e: T) {
					McOr(McArr(PaApp(PaId(IdUid(<...>)), PaStr(int_of_string)), ExNil, ExApp(ExId(IdLid(<...>)), ExApp(ExApp(<...>, <...>),
							   ExId(<...>)))), McArr(PaApp(PaId(IdUid(<...>)), PaAny), ExNil, ExApp(ExId(IdLid(<...>)), ExApp(ExApp(<...>, <...>),
												 ExId(<...>)))))									case Failure(int_of_string): err( ^ ("Index expected. Got ", n));
				case Invalid_argument(_): err( ^ ("Out-of-bounds special parameter: ", n));
				};
			case ::(Str.Delim(x), _): err( ^ ("Unexpected ", x));
				};
		};
		loop(Str.full_split(regex, code));
	};

	public static function map_source_header(com, f) return {
		switch (Common.defined_value_safe(com, Define.SourceHeader)) {
		case : [];
		case s: f(s);
		};
	}
}
