import Ast;
import Type;
import Common;
import Typecore;

class Typeload {
	public static var locate_macro_error = ref(True);

	public static function transform_abstract_field(ctx, this_t, a_t, a, f) return {
		var stat = List.mem(AStatic, f.cff_access);
		var p = f.cff_pos;
		switch (f.cff_kind) {
		case FProp(get | never, set | never, _, _) if (!(stat)):
			if (Common.defined(ctx.com, Define.As3)) {
				f.cff_meta = ::((new Tuple(Meta.Extern, [], p)), f.cff_meta);
			} else {
				[];
			};
			{
				(f) with cff_access = ::(AStatic, f.cff_access);
				cff_meta = ::((new Tuple(Meta.Impl, [], p)), f.cff_meta)
			};
		case FProp(_) if (!(stat)): display_error(ctx, "Member property accessors must be get/set or never", p);
			f;
		case FFun(fu) if (&&(=(f.cff_name, "new"), !(stat))): 	function init(p) return {
				(new Tuple(EVars(::((new Tuple("this", Some(this_t), None)), [])), p));
			};
			function cast(e) return {
				(new Tuple(ECast(e, None), pos(e)));
			};
			function check_type(e, ct) return {
				(new Tuple(ECheckType(e, ct), pos(e)));
			};
			function ret(p) return {
				(new Tuple(EReturn(Some(cast((new Tuple(EConst(Ident("this")), p))))), p));
			};
			if (Meta.has(Meta.MultiType, a.a_meta)) {
				if (List.mem(AInline, f.cff_access)) {
					error("MultiType constructors cannot be inline", f.cff_pos);
				} else {
					[];
				};
				if (<>(fu.f_expr, None)) {
					error("MultiType constructors cannot have a body", f.cff_pos);
				} else {
					[];
				};
			} else {
				[];
			};
			function has_call(e) return {
				function loop(e) return {
					switch (fst(e)) {
					case ECall(_): raise(Exit);
					case _: Ast.map_expr(loop, e);
					};
				};
				try {
					ignore(loop(e));
					False;
				} catch (e: Exit) {
					True;
				};
			};
			var fu = { (fu) with f_expr = switch (fu.f_expr) {
		case None: if (Meta.has(Meta.MultiType, a.a_meta)) {
					Some(EConst(Ident("null")), p);
				} else {
					None;
				};
			case Some((EBlock(::((EBinop(OpAssign, (EConst(Ident(this)), _), e), _), [])), _) | (EBinop(OpAssign, (EConst(Ident(this)), _), e), _)) if (!(has_call(e)))
						: Some(EReturn(Some(cast(check_type(e, this_t)))), pos(e));
			case Some(EBlock(el), p): Some(EBlock(@(::(init(p), el), ::(ret(p), []))), p);
			case Some(e): Some(EBlock(::(init(p), ::(e, ::(ret(p), [])))), p);
			};
			f_type = Some(a_t)
					 };
			{
				(f) with cff_name = "_new";
				cff_access = ::(AStatic, f.cff_access);
				cff_kind = FFun(fu);
				cff_meta = ::((new Tuple(Meta.Impl, [], p)), f.cff_meta)
			};
		case FFun(fu) if (!(stat)):
			if (Meta.has(Meta.From, f.cff_meta)) {
				error("@:from cast functions must be static", f.cff_pos);
			} else {
				[];
			};
			var fu = { (fu) with f_args = if (List.mem(AMacro, f.cff_access)) {
			fu.f_args;
		} else {
			::((new Tuple("this", False, Some(this_t), None)), fu.f_args);
			}
					 };
			{
				(f) with cff_kind = FFun(fu);
				cff_access = ::(AStatic, f.cff_access);
				cff_meta = ::((new Tuple(Meta.Impl, [], p)), f.cff_meta)
			};
		case _: f;
		};
	};

	public static function make_module(ctx, mpath, file, tdecls, loadp) return {
		var decls = ref([]);
		function make_path(name, priv) return {
			if (List.exists(function (t, _): = (snd(t_path(t)), name), decls.val)) {
				error( ^ ("Type name ", ^ (name, " is already defined in this module")), loadp);
			} else {
				[];
			};
			if (priv) {
				(new Tuple(@(fst(mpath), ::( ^ ("_", snd(mpath)), [])), name));
			} else {
				(new Tuple(fst(mpath), name));
			};
		};
		var m = { () with m_id = alloc_mid([]);
				  m_path = mpath;
				  m_types = [];
		m_extra = module_extra(Common.unique_full_path(file), Common.get_signature(ctx.com), file_time(file), if (ctx.in_macro) {
		MMacro;
	} else {
		MCode;
	})
			};
	var pt = ref(None);
		function make_decl(acc, decl) return {
			var p = snd(decl);
			var acc = switch (fst(decl)) {
			case EImport(_) | EUsing(_): switch (pt.val) {
				case None: acc;
				case Some(pt): display_error(ctx, "import and using may not appear after a type declaration", p);
					error("Previous type declaration found here", pt);
				};
			case EClass(d): if ( && ( > (String.length(d.d_name), 0), = (d.d_name0, '$'))) {
					error("Type names starting with a dollar are not allowed", p);
				} else {
					[];
				};
				pt.val = Some(p);
				var priv = List.mem(HPrivate, d.d_flags);
				var path = make_path(d.d_name, priv);
				var c = mk_class(m, path, p);
				c.cl_module = m;
				c.cl_private = priv;
				c.cl_doc = d.d_doc;
				c.cl_meta = d.d_meta;
				decls.val = ::((new Tuple(TClassDecl(c), decl)), decls.val);
				acc;
			case EEnum(d): if ( && ( > (String.length(d.d_name), 0), = (d.d_name0, '$'))) {
					error("Type names starting with a dollar are not allowed", p);
				} else {
					[];
				};
				pt.val = Some(p);
				var priv = List.mem(EPrivate, d.d_flags);
				var path = make_path(d.d_name, priv);
				var e = { () with e_path = path;
						  e_module = m;
						  e_pos = p;
						  e_doc = d.d_doc;
						  e_meta = d.d_meta;
						  e_params = [];
						  e_private = priv;
						  e_extern = List.mem(EExtern, d.d_flags);
						  e_constrs = PMap.empty;
						  e_names = [];
						  e_type = { () with t_path = (new Tuple([], ^ ("Enum<", ^ (s_type_path(path), ">"))));
									 t_module = m;
									 t_doc = None;
									 t_pos = p;
									 t_type = mk_mono([]);
									 t_private = True;
									 t_params = [];
									 t_meta = []
								   }
						};
				decls.val = ::((new Tuple(TEnumDecl(e), decl)), decls.val);
				acc;
			case ETypedef(d): if ( && ( > (String.length(d.d_name), 0), = (d.d_name0, '$'))) {
					error("Type names starting with a dollar are not allowed", p);
				} else {
					[];
				};
				pt.val = Some(p);
				var priv = List.mem(EPrivate, d.d_flags);
				var path = make_path(d.d_name, priv);
				var t = { () with t_path = path;
						  t_module = m;
						  t_pos = p;
						  t_doc = d.d_doc;
						  t_private = priv;
						  t_params = [];
						  t_type = mk_mono([]);
						  t_meta = d.d_meta
						};
				decls.val = ::((new Tuple(TTypeDecl(t), decl)), decls.val);
				acc;
			case EAbstract(d): if ( && ( > (String.length(d.d_name), 0), = (d.d_name0, '$'))) {
					error("Type names starting with a dollar are not allowed", p);
				} else {
					[];
				};
				var priv = List.mem(APrivAbstract, d.d_flags);
				var path = make_path(d.d_name, priv);
				var a = { () with a_path = path;
						  a_private = priv;
						  a_module = m;
						  a_pos = p;
						  a_doc = d.d_doc;
						  a_params = [];
						  a_meta = d.d_meta;
						  a_from = [];
						  a_to = [];
						  a_from_field = [];
						  a_to_field = [];
						  a_ops = [];
						  a_unops = [];
						  a_impl = None;
						  a_array = [];
						  a_this = mk_mono([]);
						  a_resolve = None
						};
				decls.val = ::((new Tuple(TAbstractDecl(a), decl)), decls.val);
				switch (d.d_data) {
				case [] if (Meta.has(Meta.CoreType, a.a_meta)): a.a_this = t_dynamic;
					acc;
				case fields: var a_t = var params = List.map(function t: TPType(CTPath({ () with tname = t.tp_name;
														tparams = [];
														tsub = None;
														tpackage = []
																						   })), d.d_params);
					CTPath({ () with tpackage = [];
							 tname = d.d_name;
							 tparams = params;
							 tsub = None
						   });
					function loop(match) return switch (match) {
					case []: a_t;
					case ::(AIsType(t), _): t;
					case ::(_, l): loop(l);
					};
					var this_t = loop(d.d_flags);
					var fields = List.map(transform_abstract_field(ctx, this_t, a_t, a), fields);
					var meta = ref([]);
					if (has_meta(Meta.Dce, a.a_meta)) {
						meta.val = ::((new Tuple(Meta.Dce, [], p)), meta.val);
					} else {
						[];
					};
					var acc = make_decl(acc, (new Tuple(EClass({ () with d_name = ^ (d.d_name, "_Impl_");
														d_flags = ::(HPrivate, []);
														d_data = fields;
														d_doc = None;
														d_params = [];
														d_meta = meta.val
															   }), p)));
					switch (decls.val) {
					case ::((TClassDecl(c), _), _): List.iter(function m: switch (m) {
					case (Meta.Build | Meta.CoreApi | Meta.Allow | Meta.Access | Meta.Enum | Meta.Dce | Meta.Native | Meta.Expose, _, _):
							c.cl_meta = ::(m, c.cl_meta);
						case _: [];
						}, a.a_meta);
						a.a_impl = Some(c);
						c.cl_kind = KAbstractImpl(a);
					case _: assert False;
					};
					acc;
				};
			};
			::(decl, acc);
		};
		var tdecls = List.fold_left(make_decl, [], tdecls);
		var decls = List.rev(decls.val);
		m.m_types = List.map(fst, decls);
		(new Tuple(m, decls, List.rev(tdecls)));
	};

	public static function parse_file(com, file, p) return {
		var ch = try {
			open_in_bin(file);
		} catch (e: _) {
			error( ^ ("Could not open ", file), p);
		};
		var t = Common.timer("parsing");
		Lexer.init(file, True);
		incr(stats.s_files_parsed);
		var data = try {
			Parser.parse(com, Lexing.from_channel(ch));
		} catch (e: e) {
			close_in(ch);
			t([]);
			raise(e);
		};
		close_in(ch);
		t([]);
		Common.log(com, ^ ("Parsed ", file));
		data;
	};

	public static var parse_hook = ref(parse_file);

	public static var type_module_hook = ref(function _: function _: function _: None);

	public static var type_function_params_rec = ref(function _: function _: function _: function _: assert False);

	public static var return_partial_type = ref(False);

	public static function type_function_arg(ctx, t, e, opt, p) return {
		if (opt) {
			var e = switch (e) {
			case None: Some(EConst(Ident("null")), p);
			case _: e;
			};
			(new Tuple(ctx.t.tnull(t), e));
		} else {
			var t = switch (e) {
			case Some(EConst(Ident(null)), p): ctx.t.tnull(t);
			case _: t;
			};
			(new Tuple(t, e));
		};
	};

	public static function type_var_field(ctx, t, e, stat, p) return {
		if (stat) {
			ctx.curfun = FunStatic;
		} else {
			ctx.curfun = FunMember;
		};
		var e = type_expr(ctx, e, WithType(t));
		var e = cast_or_unify_ref.val(ctx, t, e, p);
		switch (t) {
		case TType({ t_path = ([], UInt) }, []) | TAbstract({ a_path = ([], UInt) }, []) if (stat): {
			(e) with etype = t
		};
		case _: e;
		};
	};

	public static function apply_macro(ctx, mode, path, el, p) return {
		var Tuple(cpath, meth) = switch (List.rev(ExtString.String.nsplit(path, "."))) {
		case ::(meth, ::(name, pack)): (new Tuple((new Tuple(List.rev(pack), name)), meth));
		case _: error("Invalid macro path", p);
		};
		ctx.g.do_macro(ctx, mode, cpath, meth, el, p);
	};

	public static function load_type_def(ctx, p, t) return {
		var no_pack = = (t.tpackage, []);
		var tname = switch (t.tsub) {
		case None: t.tname;
		case Some(n): n;
		};
		try {
			if (<>(t.tsub, None)) {
				raise(Not_found);
			} else {
				[];
			};
			List.find(function t2: var tp = t_path(t2);
					  || ( = (tp, (new Tuple(t.tpackage, tname))), && (no_pack, = (snd(tp), tname))), @(ctx.m.curmod.m_types, ctx.m.module_types));
		} catch (e: Not_found) {
			function next([]) return {
				var Tuple(t, m) = try {
					(new Tuple(t, ctx.g.do_load_module(ctx, (new Tuple(t.tpackage, t.tname)), p)));
				} catch (e: Error(Module_not_found(_))(p2) = e) {
					switch (t.tpackage) {
					case ::(std, l): var t = { (t) with tpackage = l };
						(new Tuple(t, ctx.g.do_load_module(ctx, (new Tuple(t.tpackage, t.tname)), p)));
					case _: raise(e);
					};
				};
				var tpath = (new Tuple(t.tpackage, tname));
				try {
					List.find(function t: && (!(t_infos(t).mt_private), = (t_path(t), tpath)), m.m_types);
				} catch (e: Not_found) {
					raise(Error(Type_not_found(m.m_path, tname), p));
				};
			};
			try {
				if (!(no_pack)) {
					raise(Exit);
				} else {
					[];
				};
				function loop(match) return switch (match) {
				case []: raise(Exit);
				case ::(wp, l): try {
						load_type_def(ctx, p, { (t) with tpackage = wp });
					} catch (e: Error(Module_not_found(_), p2) | Error(Type_not_found(_), p2)) {
						loop(l);
					};
				};
				loop(ctx.m.wildcard_packages);
			} catch (e: Exit) {
				function loop(match) return switch (match) {
				case []: raise(Exit);
				case ::(_, lnext) = l: try {
						load_type_def(ctx, p, { (t) with tpackage = List.rev(l) });
					} catch (e: Error(Module_not_found(_), p2) | Error(Type_not_found(_), p2)) {
						loop(lnext);
					};
				};
				try {
					if (!(no_pack)) {
						raise(Exit);
					} else {
						[];
					};
					switch (fst(ctx.m.curmod.m_path)) {
					case []: raise(Exit);
					case ::(x, _): try {
							switch (PMap.find(x, ctx.com.package_rules)) {
							case Forbidden: raise(Exit);
							case _: [];
							};
						} catch (e: Not_found) {
							[];
						};
					};
					loop(List.rev(fst(ctx.m.curmod.m_path)));
				} catch (e: Exit) {
					next([]);
				};
			};
		};
	};

	public static function check_param_constraints(ctx, types, t, pl, c, p) return {
		switch (follow(t)) {
		case TMono(_): [];
		case _: var ctl = switch (c.cl_kind) {
			case KTypeParameter(l): l;
			case _: [];
			};
			List.iter(function ti: var ti = apply_params(types, pl, ti);
			var ti = switch (follow(ti)) {
		case TInst({ cl_kind = KGeneric } = c, pl): var Tuple(_, _, f) = ctx.g.do_build_instance(ctx, TClassDecl(c), p);
				f(pl);
			case _: ti;
			};
			try {
				unify_raise(ctx, t, ti, p);
			} catch (e: Error(Unify(l))(p)) {
				if (!(ctx.untyped)) {
					display_error(ctx, error_msg(Unify(::(Constraint_failure(s_type_path(c.cl_path)), l))), p);
				} else {
					[];
				};
			}, ctl);
		};
	};

	public static function requires_value_meta(com, co) return {
		|| (Common.defined(com, Define.DocGen), switch (co) {
	case None: False;
	case Some(c): || (c.cl_extern, Meta.has(Meta.Rtti, c.cl_meta));
		});
	};

	public static function generate_value_meta(com, co, cf, args) return {
		if (requires_value_meta(com, co)) {
			var values = List.fold_left(function acc: function (name, _, _, eo):
			switch (eo) {
		case Some(e): ::((new Tuple(name, e)), acc);
			case _: acc;
			}, [], args);
			switch (values) {
			case []: [];
			case _: cf.cf_meta = ::((new Tuple(Meta.Value, ::((new Tuple(EObjectDecl(values), cf.cf_pos)), []), cf.cf_pos)),
										cf.cf_meta);
			};
		} else {
			[];
		};
	};

	public static function load_instance(ctx, t, p, allow_no_params) return {
		try {
			if ( || (<>(t.tpackage, []), <>(t.tsub, None))) {
				raise(Not_found);
			} else {
				[];
			};
			var pt = List.assoc(t.tname, ctx.type_params);
			if (<>(t.tparams, [])) {
				error( ^ ("Class type parameter ", ^ (t.tname, " can't have parameters")), p);
			} else {
				[];
			};
			pt;
		} catch (e: Not_found) {
			var mt = load_type_def(ctx, p, t);
			var Tuple(is_generic, is_generic_build) = switch (mt) {
			case TClassDecl({ cl_kind = KGeneric }): (new Tuple(True, False));
			case TClassDecl({ cl_kind = KGenericBuild(_) }): (new Tuple(False, True));
			case _: (new Tuple(False, False));
			};
			var Tuple(types, path, f) = ctx.g.do_build_instance(ctx, mt, p);
			var is_rest = && (is_generic_build, switch (types) {
		case ::((Rest, _), []): True;
			case _: False;
			});
			if ( && (allow_no_params, && ( = (t.tparams, []), !(is_rest)))) {
				var pl = ref([]);
				pl.val = List.map(function (name, t):
				switch (follow(t)) {
			case TInst(c, _): var t = mk_mono([]);
					if ( || (<>(c.cl_kind, KTypeParameter([])), is_generic)) {
						delay(ctx, PCheckConstraint, function []: check_param_constraints(ctx, types, t, pl.val, c, p));
					} else {
						[];
					};
					t;
				case _: assert False;
				}, types);
				f(pl.val);
			} else {
				if ( = (path, (new Tuple([], "Dynamic")))) {
					switch (t.tparams) {
					case []: t_dynamic;
					case ::(TPType(t), []): TDynamic(load_complex_type(ctx, p, t));
					case _: error("Too many parameters for Dynamic", p);
					};
				} else {
					if ( && (!(is_rest), <>(List.length(types), List.length(t.tparams)))) {
						error( ^ ("Invalid number of type parameters for ", s_type_path(path)), p);
					} else {
						[];
					};
					var tparams = List.map(function t:
					switch (t) {
				case TPExpr(e): var name = switch (fst(e)) {
						case EConst(String(s)): ^ ("S", s);
						case EConst(Int(i)): ^ ("I", i);
						case EConst(Float(f)): ^ ("F", f);
						case _: "Expr";
						};
						var c = mk_class(null_module, (new Tuple([], name)), p);
						c.cl_kind = KExpr(e);
						TInst(c, []);
					case TPType(t): load_complex_type(ctx, p, t);
					}, t.tparams);
					function loop(tl1, tl2, is_rest) return {
						switch ((new Tuple(tl1, tl2))) {
						case (::(t, tl1), ::((name, t2), tl2)): 	function check_const(c) return {
								var is_expression = switch (t) {
								case TInst({ cl_kind = KExpr(_) }, _): True;
								case _: False;
								};
								var expects_expression = || ( = (name, "Const"), Meta.has(Meta.Const, c.cl_meta));
								var accepts_expression = = (name, "Rest");
								if (is_expression) {
									if ( && (!(expects_expression), !(accepts_expression))) {
										error("Constant value unexpected here", p);
									} else {
										[];
									};
								} else {
									if (expects_expression) {
										error("Constant value excepted as type parameter", p);
									} else {
										[];
									};
								};
							};
							var is_rest = || (is_rest, && ( = (name, "Rest"), is_generic_build));
							var t = switch (follow(t2)) {
							case TInst({ cl_kind = KTypeParameter([]) } = c, []) if (!(is_generic)): check_const(c);
								t;
							case TInst(c, []): check_const(c);
								var r = exc_protect(ctx, function r: r.val = function []: t;
													delay(ctx, PCheckConstraint, function []: check_param_constraints(ctx, types, t, tparams, c, p));
													t, "constraint");
								delay(ctx, PForce, function []: ignore(r.val([])));
								TLazy(r);
							case _: assert False;
							};
							::(t, loop(tl1, tl2, is_rest));
						case ([], []): [];
						case ([], ::((Rest, _), [])) if (is_generic_build): [];
						case ([], _): error( ^ ("Not enough type parameters for ", s_type_path(path)), p);
						case (::(t, tl), []): if (is_rest) {
								::(t, loop(tl, [], True));
							} else {
								error( ^ ("Too many parameters for ", s_type_path(path)), p);
							};
						};
					};
					var params = loop(tparams, types, False);
					f(params);
				};
			};
		};
	};

	public static function load_complex_type(ctx, p, t) return {
		switch (t) {
		case CTParent(t): load_complex_type(ctx, p, t);
		case CTPath(t): load_instance(ctx, t, p, False);
		case CTOptional(_): error("Optional type not allowed here", p);
		case CTExtend(tl, l): switch (load_complex_type(ctx, p, CTAnonymous(l))) {
			case TAnon(a) = ta: 	function is_redefined(cf1, a2) return {
					try {
						var cf2 = PMap.find(cf1.cf_name, a2.a_fields);
						var st = s_type(print_context([]));
						if (!(type_iseq(cf1.cf_type, cf2.cf_type))) {
							display_error(ctx, ^ ("Cannot redefine field ", ^ (cf1.cf_name, " with different type")), p);
							display_error(ctx, ^ ("First type was ", st(cf1.cf_type)), cf1.cf_pos);
							error( ^ ("Second type was ", st(cf2.cf_type)), cf2.cf_pos);
						} else {
							True;
						};
					} catch (e: Not_found) {
						False;
					};
				};
				function mk_extension(t) return {
					switch (follow(t)) {
					case TInst({ cl_kind = KTypeParameter(_) }, _): error("Cannot structurally extend type parameters", p);
					case TInst(c, tl): ctx.com.warning("Structurally extending classes is deprecated and will be removed", p);
						var c2 = mk_class(null_module, (new Tuple(fst(c.cl_path), ^ ("+", snd(c.cl_path)))), p);
						c2.cl_private = True;
						PMap.iter(function f: function _:
						try {
							ignore(class_field(c, tl, f));
							error( ^ ("Cannot redefine field ", f), p);
						} catch (e: Not_found) {
							[];
						}, a.a_fields);
						c2.cl_kind = KExtension(c, tl);
						c2.cl_super = Some(c, tl);
						c2.cl_fields = a.a_fields;
						TInst(c2, []);
					case TMono(_): error("Loop found in cascading signatures definitions. Please change order/import", p);
					case TAnon(a2): PMap.iter(function _: function cf: ignore(is_redefined(cf, a2)), a.a_fields);
						TAnon({ () with a_fields = PMap.foldi(PMap.add, a.a_fields, a2.a_fields);
								a_status = ref(Extend(::(t, [])))
							  });
					case _: error("Can only extend classes and structures", p);
					};
				};
				function loop(t) return {
					switch (follow(t)) {
					case TAnon(a2): PMap.iter(function f: function cf: if (!(is_redefined(cf, a))) {
						a.a_fields = PMap.add(f, cf, a.a_fields);
						} else {
							[];
						}, a2.a_fields);
					case _: error("Multiple structural extension is only allowed for structures", p);
					};
				};
				var il = List.map(function t: load_instance(ctx, t, p, False), tl);
				var tr = ref(None);
				var t = TMono(tr);
				var r = exc_protect(ctx, function r: r.val = function _: t;
				tr.val = Some(switch (il) {
			case ::(i, []): mk_extension(i);
				case _: List.iter(loop, il);
					a.a_status.val = Extend(il);
					ta;
				});
				t, "constraint");
				delay(ctx, PForce, function []: ignore(r.val([])));
				TLazy(r);
			case _: assert False;
			};
		case CTAnonymous(l): 	function loop(acc, f) return {
				var n = f.cff_name;
				var p = f.cff_pos;
				if (PMap.mem(n, acc)) {
					error( ^ ("Duplicate field declaration : ", n), p);
				} else {
					[];
				};
				function topt(match) return switch (match) {
				case None: error( ^ ("Explicit type required for field ", n), p);
				case Some(t): load_complex_type(ctx, p, t);
				};
				function no_expr(match) return switch (match) {
				case None: [];
				case Some(_, p): error("Expression not allowed here", p);
				};
				var pub = ref(True);
				var dyn = ref(False);
				var params = ref([]);
				List.iter(function a:
				switch (a) {
			case APublic: [];
				case APrivate: pub.val = False;
				case ADynamic if (switch (f.cff_kind) {
						case FFun(_): True;
							case _: False;
							}): dyn.val = True;
				case AStatic | AOverride | AInline | ADynamic | AMacro: error( ^ ("Invalid access ", Ast.s_access(a)), p);
				}, f.cff_access);
				var Tuple(t, access) = switch (f.cff_kind) {
				case FVar(Some(CTPath({ tpackage = []; tname = Void })), _) | FProp(_, _, Some(CTPath({ tpackage = []; tname = Void })), _)
						: error("Fields of type Void are not allowed in structures", p);
				case FVar(t, e): no_expr(e);
					(new Tuple(topt(t), Var({ () with v_read = AccNormal;
											  v_write = AccNormal
											})));
				case FFun(fd): params.val = type_function_params_rec.val(ctx, fd, f.cff_name, p);
					no_expr(fd.f_expr);
					var old = ctx.type_params;
					ctx.type_params = @(params.val, old);
					var args = List.map(function (name, o, t, e): no_expr(e);
										(new Tuple(name, o, topt(t))), fd.f_args);
					var t = (new Tuple(TFun(args, topt(fd.f_type)), Method(if (dyn.val) {
					MethDynamic;
				} else {
					MethNormal;
				})));
					ctx.type_params = old;
					t;
				case FProp(i1, i2, t, e): no_expr(e);
					function access(m, get) return {
						switch (m) {
						case null: AccNo;
						case never: AccNever;
					case default: AccNormal;
						case dynamic: AccCall;
						case get if (get): AccCall;
						case set if (!(get)): AccCall;
						case x if (&&(get, =(x, ^("get_", n)))): AccCall;
						case x if (&&(!(get), =(x, ^("set_", n)))): AccCall;
						case _: error("Custom property access is no longer supported in Haxe 3", f.cff_pos);
						};
					};
					var t = switch (t) {
					case None: error("Type required for structure property", p);
					case Some(t): t;
					};
					(new Tuple(load_complex_type(ctx, p, t), Var({ () with v_read = access(i1, True);
							   v_write = access(i2, False)
																 })));
				};
				var t = if (Meta.has(Meta.Optional, f.cff_meta)) {
					ctx.t.tnull(t);
				} else {
					t;
				};
				var cf = { () with cf_name = n;
						   cf_type = t;
						   cf_pos = p;
						   cf_public = pub.val;
						   cf_kind = access;
						   cf_params = params.val;
						   cf_expr = None;
						   cf_doc = f.cff_doc;
						   cf_meta = f.cff_meta;
						   cf_overloads = []
						 };
				init_meta_overloads(ctx, None, cf);
				PMap.add(n, cf, acc);
			};
			mk_anon(List.fold_left(loop, PMap.empty, l));
		case CTFunction(args, r): switch (args) {
			case ::(CTPath({ tpackage = []; tparams = []; tname = Void }), []): TFun([], load_complex_type(ctx, p, r));
			case _: TFun(List.map(function t: var Tuple(t, opt) = switch (t) {
			case CTOptional(t): (new Tuple(t, True));
				case _: (new Tuple(t, False));
				};
				(new Tuple("", opt, load_complex_type(ctx, p, t))), args), load_complex_type(ctx, p, r));
			};
		};
	};

	public static function init_meta_overloads(ctx, co, cf) return {
		var overloads = ref([]);
		function filter_meta(m) return {
			switch (m) {
			case (Meta.Overload | Meta.Value, _, _): False;
			case _: True;
			};
		};
		var cf_meta = List.filter(filter_meta, cf.cf_meta);
		cf.cf_meta = List.filter(function m:
		switch (m) {
	case (Meta.Overload, ::((EFunction(fname, f), p), []), _): if (<>(fname, None)) {
				error("Function name must not be part of @:overload", p);
			} else {
				[];
			};
			switch (f.f_expr) {
			case Some(EBlock([]), _): [];
			case _: error("Overload must only declare an empty method body {}", p);
			};
			var old = ctx.type_params;
			switch (cf.cf_params) {
			case []: [];
			case l: ctx.type_params = List.filter(function t: !(List.mem(t, l)), ctx.type_params);
			};
			var params = type_function_params_rec.val(ctx, f, cf.cf_name, p);
			ctx.type_params = @(params, ctx.type_params);
			function topt(match) return switch (match) {
			case None: error("Explicit type required", p);
			case Some(t): load_complex_type(ctx, p, t);
			};
			var args = List.map(function (a, opt, t, _): (new Tuple(a, opt, topt(t))), f.f_args);
			var cf = { (cf) with cf_type = TFun(args, topt(f.f_type));
					   cf_params = params;
					   cf_meta = cf_meta
					 };
			generate_value_meta(ctx.com, co, cf, f.f_args);
			overloads.val = ::(cf, overloads.val);
			ctx.type_params = old;
			False;
		case (Meta.Overload, [], _) if (ctx.com.config.pf_overload): 	function topt(Tuple(n, _, t)) return {
				switch (t) {
				case TMono(t) if (=(t.val, None)): error( ^ ("Explicit type required for overload functions\nFor function argument '",
					^ (n, "'")), cf.cf_pos);
				case _: [];
				};
			};
			switch (follow(cf.cf_type)) {
			case TFun(args, _): List.iter(topt, args);
			case _: [];
			};
			True;
		case (Meta.Overload, [], p):
			error("This platform does not support this kind of overload declaration. Try @:overload[function[]... {}] instead", p);
		case (Meta.Overload, _, p): error("Invalid @:overload metadata format", p);
		case _: True;
		}, cf.cf_meta);
		cf.cf_overloads = List.rev(overloads.val);
	};

	public static function hide_params(ctx) return {
		var old_m = ctx.m;
		var old_type_params = ctx.type_params;
		var old_deps = ctx.g.std.m_extra.m_deps;
		ctx.m = {
			() with curmod = ctx.g.std;
			module_types = [];
			module_using = [];
			module_globals = PMap.empty;
			wildcard_packages = [];
			module_imports = []
		};
		ctx.type_params = [];
		function []: ctx.m = old_m;
		ctx.type_params = old_type_params;
		ctx.g.std.m_extra.m_deps = old_deps;
	};

	public static function load_core_type(ctx, name) return {
		var show = hide_params(ctx);
		var t = load_instance(ctx, {
			() with tpackage = [];
			tname = name;
			tparams = [];
			tsub = None
		}, null_pos, False);
		show([]);
		add_dependency(ctx.m.curmod, switch (t) {
	case TInst(c, _): c.cl_module;
		case TType(t, _): t.t_module;
		case TAbstract(a, _): a.a_module;
		case TEnum(e, _): e.e_module;
		case _: assert False;
		});
		t;
	};

	public static function t_iterator(ctx) return {
		var show = hide_params(ctx);
		switch (load_type_def(ctx, null_pos, {
		() with tpackage = [];
			tname = "Iterator";
			tparams = [];
			tsub = None
		})) {
		case TTypeDecl(t): show([]);
			add_dependency(ctx.m.curmod, t.t_module);
			if (<>(List.length(t.t_params), 1)) {
				assert False;
			} else {
				[];
			};
			var pt = mk_mono([]);
			(new Tuple(apply_params(t.t_params, ::(pt, []), t.t_type), pt));
		case _: assert False;
		};
	};

	public static function load_type_opt( ? : (opt = False), ctx, p, t) return {
		var t = switch (t) {
		case None: mk_mono([]);
		case Some(t): load_complex_type(ctx, p, t);
		};
		if (opt) {
			ctx.t.tnull(t);
		} else {
			t;
		};
	};

	public static function valid_redefinition(ctx, f1, t1, f2, t2) return {
		function valid(t1, t2) return {
			Type.unify(t1, t2);
			if (<>(is_null(t1), is_null(t2))) {
				raise(Unify_error(::(Cannot_unify(t1, t2), [])));
			} else {
				[];
			};
		};
		var Tuple(t1, t2) = switch ((new Tuple(f1.cf_params, f2.cf_params))) {
		case ([], []): (new Tuple(t1, t2));
		case (l1, l2) if (=(List.length(l1), List.length(l2))): var to_check = ref([]);
			var monos = List.map2(function (name, p1): function (_, p2):
			switch ((new Tuple(follow(p1), follow(p2)))) {
		case (TInst({ cl_kind = KTypeParameter(ct1) } = c1, pl1), TInst({ cl_kind = KTypeParameter(ct2) } = c2, pl2)):
				switch ((new Tuple(ct1, ct2))) {
				case ([], []): [];
				case (_, _) if (=(List.length(ct1), List.length(ct2))): 	function check(monos) return {
						List.iter2(function t1: function t2:
						try {
							var t1 = apply_params(l1, monos, apply_params(c1.cl_params, pl1, t1));
							var t2 = apply_params(l2, monos, apply_params(c2.cl_params, pl2, t2));
							type_eq(EqStrict, t1, t2);
						} catch (e: Unify_error(l)) {
							raise(Unify_error(::(Unify_custom("Constraints differ"), l)));
						}, ct1, ct2);
					};
					to_check.val = ::(check, to_check.val);
				case _: raise(Unify_error(::(Unify_custom("Different number of constraints"), [])));
				};
			case _: [];
			};
			TInst(mk_class(null_module, (new Tuple([], name)), Ast.null_pos), []), l1, l2);
			List.iter(function f: f(monos), to_check.val);
			(new Tuple(apply_params(l1, monos, t1), apply_params(l2, monos, t2)));
		case _: (new Tuple(t1, t2));
		};
		switch ((new Tuple(f1.cf_kind, f2.cf_kind))) {
		case (Method(m1), Method(m2)) if (&&(!(=(m1, MethDynamic)), !(=(m2, MethDynamic)))):
			switch ((new Tuple(follow(t1), follow(t2)))) {
			case (TFun(args1, r1), TFun(args2, r2)): if (!( = (List.length(args1), List.length(args2)))) {
					raise(Unify_error(::(Unify_custom("Different number of function arguments"), [])));
				} else {
					[];
				};
				try {
					List.iter2(function (n, o1, a1): function (_, o2, a2):
					if (<>(o1, o2)) {
					raise(Unify_error(::(Not_matching_optional(n), [])));
					} else {
						[];
					};
					try {
						valid(a2, a1);
					} catch (e: Unify_error(_)) {
						raise(Unify_error(::(Cannot_unify(a1, a2), [])));
					}, args1, args2);
					valid(r1, r2);
				} catch (e: Unify_error(l)) {
					raise(Unify_error(::(Cannot_unify(t1, t2), l)));
				};
			case _: assert False;
			};
		case (_, Var({ v_write = AccNo | AccNever })): valid(t2, t1);
		case (_, Var({ v_read = AccNo | AccNever })): valid(t1, t2);
		case (_, _): type_eq(EqStrict, t1, t2);
			if (<>(is_null(t1), is_null(t2))) {
				raise(Unify_error(::(Cannot_unify(t1, t2), [])));
			} else {
				[];
			};
		};
	};

	public static function copy_meta(meta_src, meta_target, sl) return {
		var meta = ref(meta_target);
		List.iter(function (m, e, p):
		if (List.mem(m, sl)) {
		meta.val = ::((new Tuple(m, e, p)), meta.val);
		} else {
			[];
		}, meta_src);
		meta.val;
	};

	public static function same_overload_args(get_vmtype, t1, t2, f1, f2) return {
		var get_vmtype = switch (get_vmtype) {
		case None: function f: f;
		case Some(f): f;
		};
		if (<>(List.length(f1.cf_params), List.length(f2.cf_params))) {
			False;
		} else {
			function follow_skip_null(t) return {
				switch (t) {
				case TMono(r): switch (r.val) {
					case Some(t): follow_skip_null(t);
					case _: t;
					};
				case TLazy(f): follow_skip_null(f.val([]));
				case TType({ t_path = ([], Null) } = t, ::(p, [])): TType(t, ::(follow(p), []));
				case TType(t, tl): follow_skip_null(apply_params(t.t_params, tl, t.t_type));
				case _: t;
				};
			};
			function same_arg(t1, t2) return {
				var t1 = get_vmtype(follow_skip_null(t1));
				var t2 = get_vmtype(follow_skip_null(t2));
				switch ((new Tuple(t1, t2))) {
				case (TType(_), TType(_)): type_iseq(t1, t2);
				case (TType(_), _) | (_, TType(_)): False;
				case _: type_iseq(t1, t2);
				};
			};
			switch ((new Tuple(follow(apply_params(f1.cf_params, List.map(function (_, t): t, f2.cf_params), t1)), follow(t2)))) {
			case (TFun(a1, _), TFun(a2, _)): try {
					List.for_all2(function (_, _, t1): function (_, _, t2): same_arg(t1, t2), a1, a2);
				} catch (e: Invalid_argument(List.for_all2)) {
					False;
				};
			case _: assert False;
			};
		};
	};

	public static function get_overloads(c, i) return {
		var ret = try {
			var f = PMap.find(i, c.cl_fields);
			switch (f.cf_kind) {
			case Var(_): [];
			case Method(_): ::((new Tuple(f.cf_type, f)), List.map(function f: (new Tuple(f.cf_type, f)), f.cf_overloads));
			};
		} catch (e: Not_found) {
			[];
		};
		var rsup = switch (c.cl_super) {
		case None if (c.cl_interface): var ifaces = List.concat(List.map(function (c, tl): List.map(function (t,
					f): (new Tuple(apply_params(c.cl_params, tl, t), f)), get_overloads(c, i)), c.cl_implements));
			@(ret, ifaces);
		case None: ret;
		case Some(c, tl): @(ret, List.map(function (t, f): (new Tuple(apply_params(c.cl_params, tl, t), f)), get_overloads(c, i)));
		};
		@(ret, List.filter(function (t, f): !(List.exists(function (t2, f2): same_overload_args(t, t2, f, f2), ret)), rsup));
	};

	public static function check_overloads(ctx, c) return {
		List.iter(function f:
		if (Meta.has(Meta.Overload, f.cf_meta)) {
		List.iter(function f2:
		try {
			ignore(List.find(function f3:
			&& ( != (f3, f2), same_overload_args(f2.cf_type, f3.cf_type, f2, f3)), ::(f, f.cf_overloads)));
				display_error(ctx, ^ ("Another overloaded field of same signature was already declared : ", f2.cf_name), f2.cf_pos);
			} catch (e: Not_found) {
				[];
			}, ::(f, f.cf_overloads));
		} else {
			[];
		}, @(c.cl_ordered_fields, c.cl_ordered_statics));
	};

	public static function check_overriding(ctx, c) return {
		switch (c.cl_super) {
		case None: switch (c.cl_overrides) {
			case []: [];
			case ::(i, _): display_error(ctx, ^ ("Field ", ^ (i.cf_name, " is declared 'override' but doesn't override any field")),
				i.cf_pos);
			};
		case _ if (&&(c.cl_extern, Meta.has(Meta.CsNative, c.cl_meta))): [];
		case Some(csup, params): PMap.iter(function i: function f: var p = f.cf_pos;
			function check_field(f, get_super_field, is_overload) return {
				try {
					if ( && (is_overload, !(Meta.has(Meta.Overload, f.cf_meta)))) {
						display_error(ctx, ^ ("Missing @:overload declaration for field ", i), p);
					} else {
						[];
					};
					var Tuple(t, f2) = get_super_field(csup, i);
					switch (f2.cf_kind) {
					case Var({ v_read = AccRequire(_) }): raise(Not_found);
					case _: [];
					};
					if ( && (ctx.com.config.pf_overload, && (Meta.has(Meta.Overload, f2.cf_meta), !(Meta.has(Meta.Overload, f.cf_meta))))) {
						display_error(ctx, ^ ("Field ", ^ (i,
														   " should be declared with @:overload since it was already declared as @:overload in superclass")), p);
					} else {
						if (!(List.memq(f, c.cl_overrides))) {
							display_error(ctx, ^ ("Field ", ^ (i, ^ (" should be declared with 'override' since it is inherited from superclass ",
							Ast.s_type_path(csup.cl_path)))), p);
						} else {
							if ( && (!(f.cf_public), f2.cf_public)) {
								display_error(ctx, ^ ("Field ", ^ (i, " has less visibility [public/private] than superclass one")), p);
							} else {
								switch ((new Tuple(f.cf_kind, f2.cf_kind))) {
								case (_, Method(MethInline)): display_error(ctx, ^ ("Field ", ^ (i, " is inlined and cannot be overridden")), p);
								case (a, b) if (=(a, b)): [];
								case (Method(MethInline), Method(MethNormal)): [];
								case _: display_error(ctx, ^ ("Field ", ^ (i, " has different property access than in superclass")), p);
								};
							};
						};
					};
					if (has_meta(Meta.Final, f2.cf_meta)) {
						display_error(ctx, ^ ("Cannot override @:final method ", i), p);
					} else {
						[];
					};
					try {
						var t = apply_params(csup.cl_params, params, t);
						valid_redefinition(ctx, f, f.cf_type, f2, t);
					} catch (e: Unify_error(l)) {
						display_error(ctx, ^ ("Field ", ^ (i, " overloads parent class with different or incomplete type")), p);
						display_error(ctx, error_msg(Unify(l)), p);
					};
				} catch (e: Not_found) {
					if (List.memq(f, c.cl_overrides)) {
						var msg = if (is_overload) {
							^ ("Field ", ^ (i, " is declared 'override' but no compatible overload was found"));
						} else {
							^ ("Field ", ^ (i, " is declared 'override' but doesn't override any field"));
						};
						display_error(ctx, msg, p);
					} else {
						[];
					};
				};
			};
			if ( && (ctx.com.config.pf_overload, Meta.has(Meta.Overload, f.cf_meta))) {
			var overloads = get_overloads(csup, i);
				List.iter(function (t, f2):
				switch (f2.cf_kind) {
			case Var(_): display_error(ctx, ^ ("A variable named '", ^ (f2.cf_name, "' was already declared in a superclass")),
											   f.cf_pos);
				case _: [];
				}, overloads);
				List.iter(function f: check_field(f, function csup: function i: List.find(function (t, f2): same_overload_args(f.cf_type,
												  apply_params(csup.cl_params, params, t), f, f2), overloads), True), ::(f, f.cf_overloads));
			} else {
				check_field(f, function csup: function i: var Tuple(_, t, f2) = raw_class_field(function f: f.cf_type, csup, params, i);
							(new Tuple(t, f2)), False);
			}, c.cl_fields);
		};
	};

	public static function class_field_no_interf(c, i) return {
		try {
			var f = PMap.find(i, c.cl_fields);
			(new Tuple(f.cf_type, f));
		} catch (e: Not_found) {
			switch (c.cl_super) {
			case None: raise(Not_found);
			case Some(c, tl): var Tuple(_, t, f) = raw_class_field(function f: f.cf_type, c, tl, i);
				(new Tuple(apply_params(c.cl_params, tl, t), f));
			};
		};
	};

	public static function check_interface(ctx, c, intf, params) return {
		var p = c.cl_pos;
		function check_field(i, f) return {
			if (ctx.com.config.pf_overload) {
			List.iter(function case f2 if (!=(f, f2)): check_field(i, f2);
				case _: [], f.cf_overloads);
			} else {
				[];
			};
			var is_overload = ref(False);
			try {
				var Tuple(t2, f2) = class_field_no_interf(c, i);
				var Tuple(t2, f2) = if ( && (ctx.com.config.pf_overload, || (<>(f2.cf_overloads, []), Meta.has(Meta.Overload, f2.cf_meta)))) {
					var overloads = get_overloads(c, i);
					is_overload.val = True;
					var t = apply_params(intf.cl_params, params, f.cf_type);
					List.find(function (t1, f1): same_overload_args(t, t1, f, f1), overloads);
				} else {
					(new Tuple(t2, f2));
				};
				ignore(follow(f2.cf_type));
				var p = switch (f2.cf_expr) {
				case None: p;
				case Some(e): e.epos;
				};
				function mkind(match) return switch (match) {
				case MethNormal | MethInline: 0;
				case MethDynamic: 1;
				case MethMacro: 2;
				};
				if ( && (f.cf_public, && (!(f2.cf_public), !(Meta.has(Meta.CompilerGenerated, f.cf_meta))))) {
					display_error(ctx, ^ ("Field ", ^ (i, ^ (" should be public as requested by ", s_type_path(intf.cl_path)))), p);
				} else {
					if ( || (!(unify_kind(f2.cf_kind, f.cf_kind)), !(switch ((new Tuple(f.cf_kind, f2.cf_kind))) {
					case (Var(_), Var(_)): True;
						case (Method(m1), Method(m2)): = (mkind(m1), mkind(m2));
						case _: False;
						}))) {
						display_error(ctx, ^ ("Field ", ^ (i, ^ (" has different property access than in ", ^ (s_type_path(intf.cl_path), ^ (", [",
						^ (s_kind(f2.cf_kind), ^ (" should be ", ^ (s_kind(f.cf_kind), "]")))))))), p);
					} else {
						try {
							valid_redefinition(ctx, f2, t2, f, apply_params(intf.cl_params, params, f.cf_type));
						} catch (e: Unify_error(l)) {
							if (!( && (Meta.has(Meta.CsNative, c.cl_meta), c.cl_extern))) {
								display_error(ctx, ^ ("Field ", ^ (i, ^ (" has different type than in ", s_type_path(intf.cl_path)))), p);
								display_error(ctx, error_msg(Unify(l)), p);
							} else {
								[];
							};
						};
					};
				};
			} catch (e: T) {
				McOr(McArr(PaId(IdUid(Not_found)), ExApp(ExId(IdLid(<...>)), ExAcc(ExId(<...>), ExId(<...>))), ExLet(ReNil,
						   BiEq(PaId(<...>), ExIfe(<...>, <...>, <...>)), ExApp(ExApp(<...>, <...>), ExId(<...>)))), McArr(PaId(IdUid(Not_found)),
			ExNil, ExId(IdUid([]))))				case Not_found if (!(c.cl_interface)): var msg = if (is_overload.val) {
					var ctx = print_context([]);
					var args = switch (follow(f.cf_type)) {
					case TFun(args, _): String.concat(", ", List.map(function (n, o, t): ^ (if (o) {
						"?";
					} else {
						"";
					}, ^ (n, ^ (" : ", s_type(ctx, t)))), args));
					case _: assert False;
					};
					^ ("No suitable overload for ", ^ (i, ^ ("[ ", ^ (args, ^ (" ], as needed by ", ^ (s_type_path(intf.cl_path),
													   " was found"))))));
				} else {
					^ ("Field ", ^ (i, ^ (" needed by ", ^ (s_type_path(intf.cl_path), " is missing"))));
				};
				display_error(ctx, msg, p);
			case Not_found: [];
			};
		};
		PMap.iter(check_field, intf.cl_fields);
		List.iter(function (i2, p2): check_interface(ctx, c, i2, List.map(apply_params(intf.cl_params, params), p2)), intf.cl_implements);
	};

	public static function check_interfaces(ctx, c) return {
		switch (c.cl_path) {
		case (::(Proxy, _), _): [];
		case _ if (&&(c.cl_extern, Meta.has(Meta.CsNative, c.cl_meta))): [];
		case _: List.iter(function (intf, params): check_interface(ctx, c, intf, params), c.cl_implements);
		};
	};

	public static function return_flow(ctx, e) return {
		function error([]) return {
			display_error(ctx, Printf.sprintf("Missing return: %s", s_type(print_context([]), ctx.ret)), e.epos);
			raise(Exit);
		};
		var return_flow = return_flow(ctx);
		function uncond(e) return {
			switch (e.eexpr) {
			case TIf(_) | TWhile(_) | TSwitch(_) | TTry(_): [];
			case TReturn(_) | TThrow(_): raise(Exit);
			case _: Type.iter(uncond, e);
			};
		};
		function has_unconditional_flow(e) return {
			try {
				uncond(e);
				False;
			} catch (e: Exit) {
				True;
			};
		};
		switch (e.eexpr) {
		case TReturn(_) | TThrow(_): [];
		case TParenthesis(e) | TMeta(_, e): return_flow(e);
		case TBlock(el): function loop(match) return switch (match) {
			case []: error([]);
			case ::(e, []): return_flow(e);
			case ::(e, _) if (has_unconditional_flow(e)): [];
			case ::(_, l): loop(l);
			};
			loop(el);
		case TIf(_, e1, Some(e2)): return_flow(e1);
			return_flow(e2);
		case TSwitch(v, cases, Some(e)): List.iter(function (_, e): return_flow(e), cases);
			return_flow(e);
		case TSwitch({ eexpr = TMeta((Meta.Exhaustive, _, _), _) }, cases, None): List.iter(function (_, e): return_flow(e),
					cases);
		case TTry(e, cases): return_flow(e);
			List.iter(function (_, e): return_flow(e), cases);
		case TWhile({ eexpr = TConst(TBool(True)) }, e, _): 	function loop(e) return {
				switch (e.eexpr) {
				case TWhile(_) | TFor(_): [];
				case TBreak: error([]);
				case _: Type.iter(loop, e);
				};
			};
			loop(e);
		case _: error([]);
		};
	};

	public static function is_generic_parameter(ctx, c) return {
		try {
			ignore(List.assoc(snd(c.cl_path), ctx.curfield.cf_params));
			Meta.has(Meta.Generic, ctx.curfield.cf_meta);
		} catch (e: Not_found) {
			try {
				ignore(List.assoc(snd(c.cl_path), ctx.type_params));
				switch (ctx.curclass.cl_kind) {
				case KGeneric: True;
				case _: False;
				};
			} catch (e: Not_found) {
				False;
			};
		};
	};

	public static function check_extends(ctx, c, t, p) return {
		switch (follow(t)) {
		case TInst({ cl_path = ([], Array); cl_extern = basic_extern }, _) | TInst({ cl_path = ([], String); cl_extern = basic_extern }, _) | TInst({ cl_path = ([], Date); cl_extern = basic_extern }, _) | TInst({ cl_path = ([], Xml); cl_extern = basic_extern }, _) if (!(&&(c.cl_extern, basic_extern)))
				: error("Cannot extend basic class", p);
		case TInst(csup, params): if (is_parent(c, csup)) {
				error("Recursive class", p);
			} else {
				[];
			};
			switch (csup.cl_kind) {
			case KTypeParameter(_) if (!(is_generic_parameter(ctx, csup))): error("Cannot extend non-generic type parameters", p);
			case _: (new Tuple(csup, params));
			};
		case _: error("Should extend by using a class", p);
		};
	};

	public static function type_function_arg_value(ctx, t, c) return {
		switch (c) {
		case None: None;
		case Some(e): var p = pos(e);
			var e = ctx.g.do_optimize(ctx, type_expr(ctx, e, WithType(t)));
			unify(ctx, e.etype, t, p);
			function loop(e) return {
				switch (e.eexpr) {
				case TConst(c): Some(c);
				case TCast(e, None): loop(e);
				case _: display_error(ctx, "Parameter default value should be constant", p);
					None;
				};
			};
			loop(e);
		};
	};

	public static function get_native_repr(md, pos) return {
		var Tuple(path, meta) = switch (md) {
		case TClassDecl(cl): (new Tuple(cl.cl_path, cl.cl_meta));
		case TEnumDecl(e): (new Tuple(e.e_path, e.e_meta));
		case TTypeDecl(t): (new Tuple(t.t_path, t.t_meta));
		case TAbstractDecl(a): (new Tuple(a.a_path, a.a_meta));
		};
		function loop(acc) return {
		case ::((Meta.JavaCanonical, ::((EConst(String(pack)), _), ::((EConst(String(name)), _), [])), _), _): (new Tuple(ExtString.String.nsplit(pack, "."), name));
		case ::((Meta.Native, ::((EConst(String(name)), _), []), _), meta): loop(Ast.parse_path(name), meta);
		case ::(_, meta): loop(acc, meta);
		case []: acc;
		};
		var Tuple(pack, name) = loop(path, meta);
		switch (pack) {
		case []: (new Tuple(EConst(Ident(name)), pos));
		case ::(hd, tl): 	function loop(pack, expr) return {
				switch (pack) {
				case ::(hd, tl): loop(tl, (new Tuple(EField(expr, hd), pos)));
				case []: (new Tuple(EField(expr, name), pos));
				};
			};
			loop(tl, (new Tuple(EConst(Ident(hd)), pos)));
		};
	};

	public static function process_meta_argument( ? : (toplevel = True), ctx, expr) return {
		switch (expr.eexpr) {
		case TField(e, f): (new Tuple(EField(process_meta_argument(toplevel = False, ctx, e), field_name(f)), expr.epos));
		case TConst(TInt(i)): (new Tuple(EConst(Int(Int32.to_string(i))), expr.epos));
		case TConst(TFloat(f)): (new Tuple(EConst(Float(f)), expr.epos));
		case TConst(TString(s)): (new Tuple(EConst(String(s)), expr.epos));
		case TConst(TNull): (new Tuple(EConst(Ident("null")), expr.epos));
		case TConst(TBool(b)): (new Tuple(EConst(Ident(string_of_bool(b))), expr.epos));
		case TCast(e, _) | TMeta(_, e) | TParenthesis(e): process_meta_argument(toplevel = , ctx, e);
		case TTypeExpr(md) if (toplevel): var p = expr.epos;
			if ( = (ctx.com.platform, Cs)) {
				(new Tuple(ECall((new Tuple(EConst(Ident("typeof")), p)), ::(get_native_repr(md, expr.epos), [])), p));
			} else {
				(new Tuple(EField(get_native_repr(md, expr.epos), "class"), p));
			};
		case TTypeExpr(md): get_native_repr(md, expr.epos);
		case _: display_error(ctx, "This expression is too complex to be a strict metadata argument", expr.epos);
			(new Tuple(EConst(Ident("null")), expr.epos));
		};
	};

	public static function make_meta(ctx, texpr, extra) return {
		switch (texpr.eexpr) {
		case TNew(c, _, el): (new Tuple(ECall(get_native_repr(TClassDecl(c), texpr.epos), @(List.map(process_meta_argument(ctx),
			el), extra)), texpr.epos));
		case TTypeExpr(md): (new Tuple(ECall(get_native_repr(md, texpr.epos), extra), texpr.epos));
		case _: display_error(ctx, "Unexpected expression", texpr.epos);
			assert False;
		};
	};

	public static function field_to_type_path(ctx, e) return {
		function loop(e, pack, name) return {
			switch (e) {
			case (EField(e, f), p) if (<>(Char.lowercase(String.get(f, 0)), String.get(f, 0))):
				switch (name) {
				case [] | ::(_, []): loop(e, pack, ::(f, name));
				case _: display_error(ctx, ^ ("Unexpected ", f), p);
					raise(Exit);
				};
			case (EField(e, f), _): loop(e, ::(f, pack), name);
			case (EConst(Ident(f)), _): var Tuple(pack, name, sub) = switch (name) {
				case []: var fchar = String.get(f, 0);
					if ( = (Char.uppercase(fchar), fchar)) {
						(new Tuple(pack, f, None));
					} else {
						display_error(ctx, "A class name must start with an uppercase character", snd(e));
						raise(Exit);
					};
				case ::(name, []): (new Tuple(::(f, pack), name, None));
				case ::(name, ::(sub, [])): (new Tuple(::(f, pack), name, Some(sub)));
				case _: assert False;
				};
				{
					() with tpackage = pack;
					tname = name;
					tparams = [];
					tsub = sub
				};
			case (_, pos): display_error(ctx, "Unexpected expression when building strict meta", pos);
				raise(Exit);
			};
		};
		loop(e, [], []);
	};

	public static function handle_fields(ctx, fields_to_check, with_type_expr) return {
		List.map(function (name, expr): var pos = snd(expr);
		var field = (new Tuple(EField(with_type_expr, name), pos));
		var fieldexpr = (new Tuple(EConst(Ident(name)), pos));
		var left_side = switch (ctx.com.platform) {
	case Cs: field;
	case Java: (new Tuple(ECall(field, []), pos));
		case _: assert False;
		};
		var left = type_expr(ctx, left_side, NoValue);
				   var right = type_expr(ctx, expr, WithType(left.etype));
				   unify(ctx, left.etype, right.etype, snd(expr));
				   (new Tuple(EBinop(Ast.OpAssign, fieldexpr, process_meta_argument(ctx, right)), pos)), fields_to_check);
	};

	public static function get_strict_meta(ctx, params, pos) return {
		var pf = ctx.com.platform;
		var Tuple(changed_expr, fields_to_check, ctype) = switch (params) {
		case ::((ECall(ef, el), p), []): var Tuple(el, fields) = switch (List.rev(el)) {
			case ::((EObjectDecl(decl), _), el): (new Tuple(List.rev(el), decl));
			case _: (new Tuple(el, []));
			};
			var tpath = field_to_type_path(ctx, ef);
			if ( = (pf, Cs)) {
				(new Tuple((new Tuple(ENew(tpath, el), p)), fields, CTPath(tpath)));
			} else {
				(new Tuple(ef, fields, CTPath(tpath)));
			};
		case ::((EConst(Ident(i)), p) = expr, []): var tpath = { () with tpackage = [];
																	 tname = i;
																	 tparams = [];
																	 tsub = None
																   };
			if ( = (pf, Cs)) {
				(new Tuple((new Tuple(ENew(tpath, []), p)), [], CTPath(tpath)));
			} else {
				(new Tuple(expr, [], CTPath(tpath)));
			};
		case ::((EField(_), p) = field, []): var tpath = field_to_type_path(ctx, field);
			if ( = (pf, Cs)) {
				(new Tuple((new Tuple(ENew(tpath, []), p)), [], CTPath(tpath)));
			} else {
				(new Tuple(field, [], CTPath(tpath)));
			};
		case _: display_error(ctx,
								  "A @:strict metadata must contain exactly one parameter. Please check the documentation for more information", pos);
			raise(Exit);
		};
		var texpr = type_expr(ctx, changed_expr, NoValue);
		var with_type_expr = (new Tuple(ECheckType((new Tuple(EConst(Ident("null")), pos)), ctype), pos));
		var extra = handle_fields(ctx, fields_to_check, with_type_expr);
		(new Tuple(Meta.Meta, ::(make_meta(ctx, texpr, extra), []), pos));
	};

	public static function check_strict_meta(ctx, metas) return {
		var pf = ctx.com.platform;
		switch (pf) {
		case Cs | Java: var ret = ref([]);
		List.iter(function case (Meta.Strict, params, pos): try {
					ret.val = ::(get_strict_meta(ctx, params, pos), ret.val);
				} catch (e: Exit) {
					[];
				};
		case _: [], metas);
			ret.val;
		case _: [];
		};
	};

	public static function add_constructor(ctx, c, force_constructor, p) return {
		switch ((new Tuple(c.cl_constructor, c.cl_super))) {
		case (None, Some({ cl_constructor = Some(cfsup) } = csup, cparams)) if (&&(!(c.cl_extern), !(Meta.has(Meta.CompilerGenerated, cfsup.cf_meta))))
				: var cf = {
				(cfsup) with cf_pos = p;
				cf_meta = [];
				cf_doc = None;
				cf_expr = None
			};
			var r = exc_protect(ctx, function r: var t = mk_mono([]);
								r.val = function []: t;
								var ctx = { (ctx) with curfield = cf;
											pass = PTypeField
										  };
								ignore(follow(cfsup.cf_type));
			if (ctx.com.config.pf_overload) {
			List.iter(function cf: ignore(follow(cf.cf_type)), cf.cf_overloads);
			} else {
				[];
			};
			function map_arg(Tuple(v, def)) return {
				switch ((new Tuple(ctx.com.platform, def))) {
				case (_, Some(_)) if (!(ctx.com.config.pf_static)): (new Tuple(v, Some(TNull)));
				case (Flash, Some(TString(_))): (new Tuple(v, Some(TNull)));
				case (Cpp, Some(TString(_))): (new Tuple(v, def));
				case (Cpp, Some(_)): (new Tuple({ (v) with v_type = ctx.t.tnull(v.v_type) }, Some(TNull)));
				case _: (new Tuple(v, def));
				};
			};
			var args = switch (cfsup.cf_expr) {
		case Some({ eexpr = TFunction(f) }): List.map(map_arg, f.tf_args);
			case _: var values = get_value_meta(cfsup.cf_meta);
				switch (follow(cfsup.cf_type)) {
				case TFun(args, _): List.map(function (n, o, t): var def = try {
						type_function_arg_value(ctx, t, Some(PMap.find(n, values)));
					} catch (e: Not_found) {
						if (o) {
							Some(TNull);
						} else {
							None;
						};
					};
					map_arg((new Tuple(alloc_var(n, if (o) {
					ctx.t.tnull(t);
					} else {
						t;
					}), def))), args);
				case _: assert False;
				};
			};
			var p = c.cl_pos;
					var vars = List.map(function (v, def): (new Tuple(alloc_var(v.v_name, apply_params(csup.cl_params, cparams, v.v_type)),
										def)), args);
					var super_call = mk(TCall(mk(TConst(TSuper), TInst(csup, cparams), p), List.map(function (v, _): mk(TLocal(v), v.v_type,
											  p), vars)), ctx.t.tvoid, p);
					var constr = mk(TFunction({ () with tf_args = vars;
												tf_type = ctx.t.tvoid;
												tf_expr = super_call
											  }), TFun(List.map(function (v, c): (new Tuple(v.v_name, <>(c, None), v.v_type)), vars), ctx.t.tvoid), p);
					cf.cf_expr = Some(constr);
					cf.cf_type = t;
					unify(ctx, t, constr.etype, p);
					t, "add_constructor");
			cf.cf_type = TLazy(r);
			c.cl_constructor = Some(cf);
			delay(ctx, PForce, function []: ignore(r.val([])));
		case (None, _) if (force_constructor): var constr = mk(TFunction({ () with tf_args = [];
					tf_type = ctx.t.tvoid;
					tf_expr = mk(TBlock([]), ctx.t.tvoid, p)
																			 }), tfun([], ctx.t.tvoid), p);
			var cf = mk_field("new", constr.etype, p);
			cf.cf_expr = Some(constr);
			cf.cf_type = constr.etype;
			cf.cf_meta = ::((new Tuple(Meta.CompilerGenerated, [], p)), []);
			cf.cf_kind = Method(MethNormal);
			c.cl_constructor = Some(cf);
		case _: [];
		};
	};

	public static function set_heritance(ctx, c, herits, p) return {
		var is_lib = Meta.has(Meta.LibType, c.cl_meta);
		var ctx = {
			(ctx) with curclass = c;
			type_params = c.cl_params
		};
		var old_meta = c.cl_meta;
		function process_meta(csup) return {
			List.iter(function m:
			switch (m) {
		case (Meta.Final, _, _): if (!( || (Meta.has(Meta.Hack, c.cl_meta), switch (c.cl_kind) {
				case KTypeParameter(_): True;
					case _: False;
					}))) {
					error("Cannot extend a final class", p);
				} else {
					[];
				};
			case (Meta.AutoBuild, el, p): c.cl_meta = ::((new Tuple(Meta.Build, el, p)), ::(m, c.cl_meta));
			case _: [];
			}, csup.cl_meta);
		};
		function cancel_build(csup) return {
			c.cl_meta = old_meta;
			c.cl_array_access = None;
			c.cl_dynamic = None;
			c.cl_implements = [];
			c.cl_super = None;
			raise(Exit);
		};
		var has_interf = ref(False);
		function loop(match) return switch (match) {
		case HPrivate | HExtern | HInterface: [];
		case HExtends(t): if (<>(c.cl_super, None)) {
				error("Cannot extend several classes", p);
			} else {
				[];
			};
			var t = load_instance(ctx, t, p, False);
			var Tuple(csup, params) = check_extends(ctx, c, t, p);
			if (!(csup.cl_build([]))) {
				cancel_build(csup);
			} else {
				[];
			};
			process_meta(csup);
			if (c.cl_interface) {
				if (!(csup.cl_interface)) {
					error("Cannot extend by using a class", p);
				} else {
					[];
				};
				c.cl_implements = ::((new Tuple(csup, params)), c.cl_implements);
				if (!(has_interf.val)) {
					if (!(is_lib)) {
						delay(ctx, PForce, function []: check_interfaces(ctx, c));
					} else {
						[];
					};
					has_interf.val = True;
				} else {
					[];
				};
			} else {
				if (csup.cl_interface) {
					error("Cannot extend by using an interface", p);
				} else {
					[];
				};
				c.cl_super = Some(csup, params);
			};
		case HImplements(t): var t = load_instance(ctx, t, p, False);
			switch (follow(t)) {
			case TInst({ cl_path = ([], ArrayAccess); cl_extern = True }, ::(t, [])):
				if (<>(c.cl_array_access, None)) {
					error("Duplicate array access", p);
				} else {
					[];
				};
				c.cl_array_access = Some(t);
			case TInst(intf, params): if (!(intf.cl_build([]))) {
					cancel_build(intf);
				} else {
					[];
				};
				if (is_parent(c, intf)) {
					error("Recursive class", p);
				} else {
					[];
				};
				if (c.cl_interface) {
					error("Interfaces cannot implement another interface [use extends instead]", p);
				} else {
					[];
				};
				if (!(intf.cl_interface)) {
					error("You can only implement an interface", p);
				} else {
					[];
				};
				process_meta(intf);
				c.cl_implements = ::((new Tuple(intf, params)), c.cl_implements);
				if ( && (!(has_interf.val), && (!(is_lib), !(Meta.has(Meta.Custom("$do_not_check_interf"), c.cl_meta))))) {
					delay(ctx, PForce, function []: check_interfaces(ctx, c));
					has_interf.val = True;
				} else {
					[];
				};
			case TDynamic(t): if (<>(c.cl_dynamic, None)) {
					error("Cannot have several dynamics", p);
				} else {
					[];
				};
				c.cl_dynamic = Some(t);
			case _: error("Should implement by using an interface", p);
			};
		};
		function resolve_imports(t) return {
			switch (t.tpackage) {
			case ::(_, _): t;
			case []: try {
					var find = List.find(function lt: = (snd(t_path(lt)), t.tname));
					var lt = try {
						find(ctx.m.curmod.m_types);
					} catch (e: Not_found) {
						find(ctx.m.module_types);
					};
					{
						(t) with tpackage = fst(t_path(lt))
					};
				} catch (e: Not_found) {
					t;
				};
			};
		};
	var herits = List.map(function case HExtends(t): HExtends(resolve_imports(t));
							  case HImplements(t): HImplements(resolve_imports(t));
								  case h: h, herits);
		List.iter(loop, List.filter(ctx.g.do_inherit(ctx, c, p), herits));
	};

	public static function type_type_param( ? : (enum_constructor = False), ctx, path, get_params, p, tp) return {
		var n = tp.tp_name;
		var c = mk_class(ctx.m.curmod, (new Tuple(@(fst(path), ::(snd(path), [])), n)), p);
		c.cl_params = type_type_params(ctx, c.cl_path, get_params, p, tp.tp_params);
		c.cl_kind = KTypeParameter([]);
		c.cl_meta = tp.Ast.tp_meta;
		if (enum_constructor) {
			c.cl_meta = ::((new Tuple(Meta.EnumConstructorParam, [], c.cl_pos)), c.cl_meta);
		} else {
			[];
		};
		var t = TInst(c, List.map(snd, c.cl_params));
		switch (tp.tp_constraints) {
		case []: (new Tuple(n, t));
		case _: var r = exc_protect(ctx, function r: r.val = function _: t;
										var ctx = { (ctx) with type_params = @(ctx.type_params, get_params([])) };
										var constr = List.map(load_complex_type(ctx, p), tp.tp_constraints);
			function loop(t) return {
				switch (follow(t)) {
				case TInst(c2, _) if (==(c, c2)): error("Recursive constraint parameter is not allowed", p);
				case TInst({ cl_kind = KTypeParameter(cl) }, _): List.iter(loop, cl);
				case _: [];
				};
			};
			List.iter(loop, constr);
			c.cl_kind = KTypeParameter(constr);
						t, "constraint");
			delay(ctx, PForce, function []: ignore(r.val([])));
			(new Tuple(n, TLazy(r)));
		};
	};

	public static function type_type_params( ? : (enum_constructor = False), ctx, path, get_params, p, tpl) return {
		var names = ref([]);
		List.map(function tp:
		if (List.mem(tp.tp_name, names.val)) {
		display_error(ctx, ^ ("Duplicate type parameter name: ", tp.tp_name), p);
		} else {
			[];
		};
		names.val = ::(tp.tp_name, names.val);
					type_type_param(enum_constructor = , ctx, path, get_params, p, tp), tpl);
	};

	public static function type_function_params(ctx, fd, fname, p) return {
		var params = ref([]);
		params.val = type_type_params(ctx, (new Tuple([], fname)), function []: params.val, p, fd.f_params);
		params.val;
	};

	public static function find_enclosing(com, e) return {
		var display_pos = ref(Parser.resume_display.val);
		function mk_null(p) return {
			(new Tuple(EDisplay((new Tuple(EConst(Ident("null")), p)), False), p));
		};
		function encloses_display_pos(p) return {
			if ( && ( <= (p.pmin, display_pos.val.pmin), >= (p.pmax, display_pos.val.pmax))) {
				var p = display_pos.val;
				display_pos.val = {
					() with pfile = "";
					pmin = -2;
					pmax = -2
				};
				Some(p);
			} else {
				None;
			};
		};
		function loop(e) return {
			switch (fst(e)) {
			case EBlock(el): var p = pos(e);
				var el = List.map(loop, el);
				var el = switch (encloses_display_pos(p)) {
				case None: el;
				case Some(p2): var Tuple(b, el) = List.fold_left(function (b, el): function e: var p = pos(e);
					if ( || (b, <= (p.pmax, p2.pmin))) {
					(new Tuple(b, ::(e, el)));
					} else {
						var e_d = (new Tuple(EDisplay(mk_null(p), False), p));
						(new Tuple(True, ::(e, ::(e_d, el))));
					}, (new Tuple(False, [])), el);
					var el = if (b) {
						el;
					} else {
						::(mk_null(p), el);
					};
					List.rev(el);
				};
				(new Tuple(EBlock(el), pos(e)));
			case _: Ast.map_expr(loop, e);
			};
		};
		loop(e);
	};

	public static function find_before_pos(com, e) return {
		var display_pos = ref(Parser.resume_display.val);
		function is_annotated(p) return {
			if ( = (p.pmax, -(display_pos.val.pmin, 1))) {
				display_pos.val = {
					() with pfile = "";
					pmin = -2;
					pmax = -2
				};
				True;
			} else {
				False;
			};
		};
		function loop(e) return {
			if (is_annotated(pos(e))) {
				(new Tuple(EDisplay(e, False), pos(e)));
			} else {
				e;
			};
		};
		function map(e) return {
			loop(Ast.map_expr(map, e));
		};
		map(e);
	};

	public static function type_function(ctx, args, ret, fmode, f, do_display, p) return {
		var locals = save_locals(ctx);
		var fargs = List.map(function (n, c, t):
		if ( = (n0, '$')) {
		error("Function argument names starting with a dollar are not allowed", p);
		} else {
			[];
		};
		var c = type_function_arg_value(ctx, t, c);
				var Tuple(v, c) = (new Tuple(add_local(ctx, n, t), c));
		if ( = (n, "this")) {
		v.v_meta = ::((new Tuple(Meta.This, [], p)), v.v_meta);
		} else {
			[];
		};
		(new Tuple(v, c)), args);
		var old_ret = ctx.ret;
		var old_fun = ctx.curfun;
		var old_opened = ctx.opened;
		ctx.curfun = fmode;
		ctx.ret = ret;
		ctx.opened = [];
		var e = switch (f.f_expr) {
		case None: error("Function body required", p);
		case Some(e): e;
		};
		var e = if (!(do_display)) {
			type_expr(ctx, e, NoValue);
		} else {
			var e = switch (ctx.com.display) {
			case DMToplevel: find_enclosing(ctx.com, e);
			case DMPosition | DMUsage | DMType: find_before_pos(ctx.com, e);
			case _: e;
			};
			try {
				if (Common.defined(ctx.com, Define.NoCOpt)) {
					raise(Exit);
				} else {
					[];
				};
				type_expr(ctx, Optimizer.optimize_completion_expr(e), NoValue);
			} catch (e: T) {
				McOr(McArr(PaOrp(PaApp(PaApp(<...>, <...>), PaAny), PaId(IdUid(<...>))), ExNil, ExApp(ExApp(ExApp(<...>, <...>),
						   ExId(<...>)), ExId(IdUid(<...>)))), McArr(PaApp(PaId(IdUid(<...>)), PaApp(PaApp(<...>, <...>), PaId(<...>))),
								   ExMat(ExApp(ExId(<...>), ExId(<...>)), McOr(McArr(<...>, <...>, <...>), McArr(<...>, <...>, <...>))),
								   ExApp(ExApp(ExApp(<...>, <...>), ExIfe(<...>, <...>, <...>)),
									 ExId(IdUid(<...>)))))				case Parser.TypePath(_, None, _) | Exit: type_expr(ctx, e, NoValue);
			case DisplayTypes(::(t, [])) if (switch (follow(t)) {
					case TMono(_): True;
						case _: False;
						}): type_expr(ctx, if ( = (ctx.com.display, DMToplevel)) {
				find_enclosing(ctx.com, e);
				} else {
					e;
				}, NoValue);
			};
		};
		var e = switch (e.eexpr) {
		case TMeta((Meta.MergeBlock, _, _), { eexpr = TBlock(el) } = e1): e1;
		case _: e;
		};
		function has_return(e) return {
			function loop(e) return {
				switch (e.eexpr) {
				case TReturn(Some(_)): raise(Exit);
				case TFunction(_): [];
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
		switch (follow(ret)) {
		case TAbstract({ a_path = ([], Void) }, _): [];
		case TMono(t) if (!(has_return(e))): ignore(link(t, ret, ctx.t.tvoid));
		case _: try {
				return_flow(ctx, e);
			} catch (e: Exit) {
				[];
			};
		};
		function loop(e) return {
			switch (e.eexpr) {
			case TCall({ eexpr = TConst(TSuper) }, _): raise(Exit);
			case TFunction(_): [];
			case _: Type.iter(loop, e);
			};
		};
		function has_super_constr([]) return {
			switch (ctx.curclass.cl_super) {
			case None: None;
			case Some(csup, tl): try {
					var Tuple(_, cf) = get_constructor(function f: f.cf_type, csup);
					Some(Meta.has(Meta.CompilerGenerated, cf.cf_meta), TInst(csup, tl));
				} catch (e: Not_found) {
					None;
				};
			};
		};
		var e = if (<>(fmode, FunConstructor)) {
			e;
		} else {
			switch (has_super_constr([])) {
			case Some(was_forced, t_super): try {
					loop(e);
					if (was_forced) {
						var e_super = mk(TConst(TSuper), t_super, e.epos);
						var e_super_call = mk(TCall(e_super, []), ctx.t.tvoid, e.epos);
						concat(e_super_call, e);
					} else {
						display_error(ctx, "Missing super constructor call", p);
						e;
					};
				} catch (e: Exit) {
					e;
				};
			case None: e;
			};
		};
		locals([]);
		var e = switch ((new Tuple(ctx.curfun, ctx.vthis))) {
		case (FunMember | FunConstructor, Some(v)): var ev = mk(TVar(v, Some(mk(TConst(TThis), ctx.tthis, p))), ctx.t.tvoid, p);
			switch (e.eexpr) {
			case TBlock(l): {
				(e) with eexpr = TBlock(::(ev, l))
			};
			case _: mk(TBlock(::(ev, ::(e, []))), e.etype, p);
			};
		case _: e;
		};
		List.iter(function r: r.val = Closed, ctx.opened);
		ctx.ret = old_ret;
		ctx.curfun = old_fun;
		ctx.opened = old_opened;
		(new Tuple(e, fargs));
	};

	public static function load_core_class(ctx, c) return {
		var ctx2 = switch (ctx.g.core_api) {
		case None: var com2 = Common.clone(ctx.com);
			com2.defines = PMap.empty;
			Common.define(com2, Define.CoreApi);
			Common.define(com2, Define.Sys);
			if (ctx.in_macro) {
				Common.define(com2, Define.Macro);
			} else {
				[];
			};
			com2.class_path = ctx.com.std_path;
			var ctx2 = ctx.g.do_create(com2);
			ctx.g.core_api = Some(ctx2);
			ctx2;
		case Some(c): c;
		};
		var tpath = switch (c.cl_kind) {
		case KAbstractImpl(a): {
			() with tpackage = fst(a.a_path);
			tname = snd(a.a_path);
			tparams = [];
			tsub = None
		};
		case _: {
			() with tpackage = fst(c.cl_path);
			tname = snd(c.cl_path);
			tparams = [];
			tsub = None
		};
		};
		var t = load_instance(ctx2, tpath, c.cl_pos, True);
		flush_pass(ctx2, PFinal, "core_final");
		switch (t) {
		case TInst(ccore, _) | TAbstract({ a_impl = Some(ccore) }, _): ccore;
		case _: assert False;
		};
	};

	public static function init_core_api(ctx, c) return {
		var ccore = load_core_class(ctx, c);
		try {
			List.iter2(function (n1, t1): function (n2, t2):
			switch ((new Tuple(follow(t1), follow(t2)))) {
		case (TInst({ cl_kind = KTypeParameter(l1) }, _), TInst({ cl_kind = KTypeParameter(l2) }, _)):
				try {
					List.iter2(function t1: function t2: type_eq(EqCoreType, t2, t1), l1, l2);
				} catch (e: T) {
					McOr(McArr(PaApp(PaId(IdUid(<...>)), PaAny), ExNil, ExApp(ExApp(ExId(<...>), ExStr(<...>)), ExAcc(ExId(<...>),
							   ExId(<...>)))), McArr(PaApp(PaId(IdUid(<...>)), PaId(IdLid(<...>))), ExNil, ExSeq(ExSem(ExApp(<...>, <...>), ExApp(<...>,
												 <...>)))))						case Invalid_argument(_): error("Type parameters must have the same number of constraints as core type",
															 c.cl_pos);
				case Unify_error(l): display_error(ctx, ^ ("Type parameter ", ^ (n2, " has different constraint than in core type")),
													   c.cl_pos);
					display_error(ctx, error_msg(Unify(l)), c.cl_pos);
				};
			case (t1, t2): Printf.printf("%s %s", s_type(print_context([]), t1), s_type(print_context([]), t2));
				assert False;
			}, ccore.cl_params, c.cl_params);
		} catch (e: Invalid_argument(_)) {
			error("Class must have the same number of type parameters as core type", c.cl_pos);
		};
		switch (c.cl_doc) {
		case None: c.cl_doc = ccore.cl_doc;
		case Some(_): [];
		};
		function compare_fields(f, f2) return {
			var p = switch (f2.cf_expr) {
			case None: c.cl_pos;
			case Some(e): e.epos;
			};
			try {
				type_eq(EqCoreType, apply_params(ccore.cl_params, List.map(snd, c.cl_params), f.cf_type), f2.cf_type);
			} catch (e: Unify_error(l)) {
				display_error(ctx, ^ ("Field ", ^ (f.cf_name, " has different type than in core type")), p);
				display_error(ctx, error_msg(Unify(l)), p);
			};
			if (<>(f2.cf_public, f.cf_public)) {
				error( ^ ("Field ", ^ (f.cf_name, " has different visibility than core type")), p);
			} else {
				[];
			};
			switch (f2.cf_doc) {
			case None: f2.cf_doc = f.cf_doc;
			case Some(_): [];
			};
			if (<>(f2.cf_kind, f.cf_kind)) {
				switch ((new Tuple(f2.cf_kind, f.cf_kind))) {
				case (Method(MethInline), Method(MethNormal)): [];
				case (Method(MethNormal), Method(MethInline)): [];
				case _: error( ^ ("Field ", ^ (f.cf_name, " has different property access than core type")), p);
				};
			} else {
				[];
			};
			switch ((new Tuple(follow(f.cf_type), follow(f2.cf_type)))) {
			case (TFun(pl1, _), TFun(pl2, _)): if ( != (List.length(pl1), List.length(pl2))) {
					error("Argument count mismatch", p);
				} else {
					[];
				};
				List.iter2(function (n1, _, _): function (n2, _, _):
				if (<>(n1, n2)) {
				error( ^ ("Method parameter name '", ^ (n2, ^ ("' should be '", ^ (n1, "'")))), p);
				} else {
					[];
				}, pl1, pl2);
			case _: [];
			};
		};
		function check_fields(fcore, fl) return {
			PMap.iter(function i: function f:
			if (!(f.cf_public)) {
			[];
			} else {
				var f2 = try {
					PMap.find(f.cf_name, fl);
				} catch (e: Not_found) {
					error( ^ ("Missing field ", ^ (i, " required by core type")), c.cl_pos);
				};
				compare_fields(f, f2);
			}, fcore);
			PMap.iter(function i: function f: var p = switch (f.cf_expr) {
		case None: c.cl_pos;
		case Some(e): e.epos;
			};
			if ( && (f.cf_public, && (!(Meta.has(Meta.Hack, f.cf_meta)), && (!(PMap.mem(f.cf_name, fcore)), !(List.memq(f, c.cl_overrides)))))) {
			error( ^ ("Public field ", ^ (i, " is not part of core type")), p);
			} else {
				[];
			}, fl);
		};
		check_fields(ccore.cl_fields, c.cl_fields);
		check_fields(ccore.cl_statics, c.cl_statics);
		switch ((new Tuple(ccore.cl_constructor, c.cl_constructor))) {
		case (None, None): [];
		case (Some({ cf_public = False }), _): [];
		case (Some(f), Some(f2)): compare_fields(f, f2);
		case (None, Some({ cf_public = False })): [];
		case _: error("Constructor differs from core type", c.cl_pos);
		};
	};

	public static function check_global_metadata(ctx, f_add, mpath, tpath, so) return {
		var sl1 = full_dot_path(mpath, tpath);
		var Tuple(sl1, field_mode) = switch (so) {
		case None: (new Tuple(sl1, False));
		case Some(s): (new Tuple(@(sl1, ::(s, [])), True));
		};
		List.iter(function (sl2, m, (recursive, to_types, to_fields)): var add = && ( || ( && (field_mode, to_fields), && (!(field_mode), to_types)), match_path(recursive, sl1, sl2));
		if (add) {
		f_add(m);
		} else {
			[];
		}, ctx.g.global_metadata);
	};

	public static function patch_class(ctx, c, fields) return {
		var path = switch (c.cl_kind) {
		case KAbstractImpl(a): a.a_path;
		case _: c.cl_path;
		};
		var h = try {
			Some(Hashtbl.find(ctx.g.type_patches, path));
		} catch (e: Not_found) {
			None;
		};
		switch (h) {
		case None: fields;
		case Some(h, hcl): c.cl_meta = @(c.cl_meta, hcl.tp_meta);
			function loop(acc) return {
			case []: acc;
			case ::(f, l): switch (f.cff_kind) {
				case FFun(ff): 	function param(Tuple(n, opt, t, e) = p) return {
						try {
							var t2 = try {
								Hashtbl.find(h, (new Tuple( ^ ("$", ^ (f.cff_name, ^ ("__", n))), False)));
							} catch (e: Not_found) {
								Hashtbl.find(h, (new Tuple( ^ ("$", n), False)));
							};
							(new Tuple(n, opt, t2.tp_type, e));
						} catch (e: Not_found) {
							p;
						};
					};
					f.cff_kind = FFun({ (ff) with f_args = List.map(param, ff.f_args) });
				case _: [];
				};
				switch (try {
					Some(Hashtbl.find(h, (new Tuple(f.cff_name, List.mem(AStatic, f.cff_access)))));
					} catch (e: Not_found) {
						None;
					}) {
				case None: loop(::(f, acc), l);
				case Some({ tp_remove = True }): loop(acc, l);
				case Some(p): f.cff_meta = @(f.cff_meta, p.tp_meta);
					switch (p.tp_type) {
					case None: [];
					case Some(t): f.cff_kind = switch (f.cff_kind) {
						case FVar(_, e): FVar(Some(t), e);
						case FProp(get, set, _, eo): FProp(get, set, Some(t), eo);
						case FFun(f): FFun({ (f) with f_type = Some(t) });
						};
					};
					loop(::(f, acc), l);
				};
			};
			List.rev(loop([], fields));
		};
	};

	public static function string_list_of_expr_path(Tuple(e, p)) return {
		try {
			string_list_of_expr_path_raise((new Tuple(e, p)));
		} catch (e: Exit) {
			error("Invalid path", p);
		};
	};

	public static function build_enum_abstract(ctx, c, a, fields, p) return {
		List.iter(function field:
		switch (field.cff_kind) {
	case FVar(ct, eo) if (!(List.mem(AStatic, field.cff_access))): field.cff_access = ::(AStatic, ::(APublic,
			::(AInline, [])));
			field.cff_meta = ::((new Tuple(Meta.Enum, [], field.cff_pos)), ::((new Tuple(Meta.Impl, [], field.cff_pos)),
			field.cff_meta));
			var e = switch (eo) {
			case None: error("Value required", field.cff_pos);
			case Some(e): (new Tuple(ECast(e, None), field.cff_pos));
			};
			field.cff_kind = FVar(ct, Some(e));
		case _: [];
		}, fields);
		(new Tuple(EVars(::((new Tuple("", Some(CTAnonymous(fields)), None)), [])), p));
	};

	public static function is_java_native_function(meta) return {
		try {
			switch (Meta.get(Meta.Native, meta)) {
			case (Meta.Native, [], _): True;
			case _: False;
			};
		} catch (e: Not_found) {
			False;
		};
	};

	public static function build_module_def(ctx, mt, meta, fvars, context_init, fbuild) return {
		function loop(Tuple(f_build, f_enum)) return {
		case (Meta.Build, args, p): (new Tuple(Some(function []: var Tuple(epath, el) = switch (args) {
		case ::((ECall(epath, el), p), []): (new Tuple(epath, el));
			case _: error("Invalid build parameters", p);
			};
			var s = try {
				String.concat(".", List.rev(string_list_of_expr_path(epath)));
			} catch (e: Error(_)(p)) {
				error("Build call parameter must be a class path", p);
			};
			if (ctx.in_macro) {
			error("You cannot use @:build inside a macro : make sure that your enum is not used in macro", p);
			} else {
				[];
			};
			var old = ctx.g.get_build_infos;
					  ctx.g.get_build_infos = function []: Some(mt, List.map(snd, t_infos(mt).mt_params), fvars([]));
					  context_init([]);
			var r = try {
				apply_macro(ctx, MBuild, s, el, p);
			} catch (e: e) {
				ctx.g.get_build_infos = old;
				raise(e);
			};
			ctx.g.get_build_infos = old;
			switch (r) {
		case None: error("Build failure", p);
			case Some(e): fbuild(e);
			}), f_enum));
		case (Meta.Enum, _, p): (new Tuple(f_build, Some(function []: switch (mt) {
		case TClassDecl({ cl_kind = KAbstractImpl(a) } = c): context_init([]);
				var e = build_enum_abstract(ctx, c, a, fvars([]), p);
				fbuild(e);
			case _: [];
			})));
		case _: (new Tuple(f_build, f_enum));
		};
		var Tuple(f_build, f_enum) = List.fold_left(loop, (new Tuple(None, None)), meta);
		switch (f_build) {
		case None: [];
		case Some(f): f([]);
		};
		switch (f_enum) {
		case None: [];
		case Some(f): f([]);
		};
	};

	public static function resolve_typedef(t) return {
		switch (t) {
		case TClassDecl(_) | TEnumDecl(_) | TAbstractDecl(_): t;
		case TTypeDecl(td): switch (follow(td.t_type)) {
			case TEnum(e, _): TEnumDecl(e);
			case TInst(c, _): TClassDecl(c);
			case TAbstract(a, _): TAbstractDecl(a);
			case _: t;
			};
		};
	};

	public static function add_module(ctx, m, p) return {
		function decl_type(t) return {
			var t = t_infos(t);
			try {
				var m2 = Hashtbl.find(ctx.g.types_module, t.mt_path);
				if ( && (<>(m.m_path, m2), = (String.lowercase(s_type_path(m2)), String.lowercase(s_type_path(m.m_path))))) {
					error( ^ ("Module ", ^ (s_type_path(m2), ^ (" is loaded with a different case than ", s_type_path(m.m_path)))), p);
				} else {
					[];
				};
				error( ^ ("Type name ", ^ (s_type_path(t.mt_path), ^ (" is redefined from module ", s_type_path(m2)))), p);
			} catch (e: Not_found) {
				Hashtbl.add(ctx.g.types_module, t.mt_path, m.m_path);
			};
		};
		List.iter(decl_type, m.m_types);
		Hashtbl.add(ctx.g.modules, m.m_path, m);
	};

	public static function init_module_type(ctx, context_init, do_init, Tuple(decl, p)) return {
		function get_type(name) return {
			try {
				List.find(function t: = (snd(t_infos(t).mt_path), name), ctx.m.curmod.m_types);
			} catch (e: Not_found) {
				assert False;
			};
		};
		switch (decl) {
		case EImport(path, mode): ctx.m.module_imports = ::((new Tuple(path, mode)), ctx.m.module_imports);
			function loop(acc) return {
			case ::(x, l) if (is_lower_ident(fst(x))): loop(::(x, acc), l);
			case rest: (new Tuple(List.rev(acc), rest));
			};
			var Tuple(pack, rest) = loop([], path);
			switch (rest) {
			case []: switch (mode) {
				case IAll: ctx.m.wildcard_packages = ::(List.map(fst, pack), ctx.m.wildcard_packages);
				case _: switch (List.rev(path)) {
					case []: assert False;
					case ::((_, p), _): error("Module name must start with an uppercase letter", p);
					};
				};
			case ::((tname, p2), rest): var p1 = switch (pack) {
				case []: p2;
				case ::((_, p1), _): p1;
				};
				var p_type = punion(p1, p2);
				var md = ctx.g.do_load_module(ctx, (new Tuple(List.map(fst, pack), tname)), p_type);
				var types = md.m_types;
				function no_private(t) return {
					!(t_infos(t).mt_private);
				};
				function chk_private(t, p) return {
					if (t_infos(t).mt_private) {
						error("You can't import a private type", p);
					} else {
						[];
					};
				};
				function has_name(name, t) return {
					= (snd(t_infos(t).mt_path), name);
				};
				function get_type(tname) return {
					var t = try {
						List.find(has_name(tname), types);
					} catch (e: Not_found) {
						error(string_error(tname, List.map(function mt: snd(t_infos(mt).mt_path), types), ^ ("Module ", ^ (s_type_path(md.m_path),
						^ (" does not define type ", tname)))), p_type);
					};
					chk_private(t, p_type);
					t;
				};
				function rebind(t, name) return {
					if (!( && ( >= (name0, 'A'), <= (name0, 'Z')))) {
						error("Type aliases must start with an uppercase letter", p);
					} else {
						[];
					};
					var Tuple(_, _, f) = ctx.g.do_build_instance(ctx, t, p_type);
					TTypeDecl({ () with t_path = (new Tuple(@(fst(md.m_path), ::( ^ ("_", snd(md.m_path)), [])), name));
								t_module = md;
								t_pos = p;
								t_private = True;
								t_doc = None;
								t_meta = [];
								t_params = t_infos(t).mt_params;
								t_type = f(List.map(snd, t_infos(t).mt_params))
							  });
				};
				function add_static_init(t, name, s) return {
					var name = switch (name) {
					case None: s;
					case Some(n): n;
					};
					switch (resolve_typedef(t)) {
					case TClassDecl(c): ignore(c.cl_build([]));
						ignore(PMap.find(s, c.cl_statics));
						ctx.m.module_globals = PMap.add(name, (new Tuple(TClassDecl(c), s)), ctx.m.module_globals);
					case TEnumDecl(e): ignore(PMap.find(s, e.e_constrs));
						ctx.m.module_globals = PMap.add(name, (new Tuple(TEnumDecl(e), s)), ctx.m.module_globals);
					case _: raise(Not_found);
					};
				};
				switch (mode) {
				case INormal | IAsName(_): var name = switch (mode) {
					case IAsName(n): Some(n);
					case _: None;
					};
					switch (rest) {
					case []: switch (name) {
						case None: ctx.m.module_types = @(List.filter(no_private, types), ctx.m.module_types);
						case Some(newname): ctx.m.module_types = ::(rebind(get_type(tname), newname), ctx.m.module_types);
						};
					case ::((tsub, p2), []): var p = punion(p1, p2);
						try {
							var tsub = List.find(has_name(tsub), types);
							chk_private(tsub, p);
							ctx.m.module_types = ::(switch (name) {
						case None: tsub;
						case Some(n): rebind(tsub, n);
							}, ctx.m.module_types);
						} catch (e: Not_found) {
							var tmain = get_type(tname);
							context_init.val = ::(function []:
							try {
								add_static_init(tmain, name, tsub);
							} catch (e: Not_found) {
								error( ^ (s_type_path(t_infos(tmain).mt_path), ^ (" has no field or subtype ", tsub)), p);
							}, context_init.val);
						};
					case ::((tsub, p2), ::((fname, p3), rest)): switch (rest) {
						case []: [];
						case ::((n, p), _): error( ^ ("Unexpected ", n), p);
						};
						var tsub = get_type(tsub);
						context_init.val = ::(function []:
						try {
							add_static_init(tsub, name, fname);
						} catch (e: Not_found) {
							error( ^ (s_type_path(t_infos(tsub).mt_path), ^ (" has no field ", fname)), punion(p, p3));
						}, context_init.val);
					};
				case IAll: var t = switch (rest) {
					case []: get_type(tname);
					case ::((tsub, _), []): get_type(tsub);
					case ::(_, ::((n, p), _)): error( ^ ("Unexpected ", n), p);
					};
					context_init.val = ::(function []:
					switch (resolve_typedef(t)) {
				case TClassDecl(c) | TAbstractDecl({ a_impl = Some(c) }): ignore(c.cl_build([]));
						PMap.iter(function _: function cf:
						if (!(has_meta(Meta.NoImportGlobal, cf.cf_meta))) {
						ctx.m.module_globals = PMap.add(cf.cf_name, (new Tuple(TClassDecl(c), cf.cf_name)), ctx.m.module_globals);
						} else {
							[];
						}, c.cl_statics);
					case TEnumDecl(e): PMap.iter(function _: function c: if (!(has_meta(Meta.NoImportGlobal, c.ef_meta))) {
						ctx.m.module_globals = PMap.add(c.ef_name, (new Tuple(TEnumDecl(e), c.ef_name)), ctx.m.module_globals);
						} else {
							[];
						}, e.e_constrs);
					case _: error("No statics to import from this type", p);
					}, context_init.val);
				};
			};
		case EUsing(t): var types = switch (t.tsub) {
			case None: var md = ctx.g.do_load_module(ctx, (new Tuple(t.tpackage, t.tname)), p);
				var types = List.filter(function t: !(t_infos(t).mt_private), md.m_types);
				ctx.m.module_types = @(types, ctx.m.module_types);
				types;
			case Some(_): var t = load_type_def(ctx, p, t);
				ctx.m.module_types = ::(t, ctx.m.module_types);
				::(t, []);
			};
			function filter_classes(types) return {
				function loop(acc, types) return {
					switch (types) {
					case ::(td, l): switch (resolve_typedef(td)) {
						case TClassDecl(c) | TAbstractDecl({ a_impl = Some(c) }): loop(::(c, acc), l);
						case td: loop(acc, l);
						};
					case []: acc;
					};
				};
				loop([], types);
			};
			context_init.val = ::(function []: ctx.m.module_using = @(filter_classes(types), ctx.m.module_using), context_init.val);
		case EClass(d): var c = switch (get_type(d.d_name)) {
			case TClassDecl(c): c;
			case _: assert False;
			};
			check_global_metadata(ctx, function m: c.cl_meta = ::(m, c.cl_meta), c.cl_module.m_path, c.cl_path, None);
			var herits = d.d_flags;
			if ( && (Meta.has(Meta.Generic, c.cl_meta), <>(c.cl_params, []))) {
				c.cl_kind = KGeneric;
			} else {
				[];
			};
			if (Meta.has(Meta.GenericBuild, c.cl_meta)) {
				c.cl_kind = KGenericBuild(d.d_data);
			} else {
				[];
			};
			if ( = (c.cl_path, (new Tuple(::("haxe", ::("macro", [])), "MacroType")))) {
				c.cl_kind = KMacroType;
			} else {
				[];
			};
			c.cl_extern = List.mem(HExtern, herits);
			c.cl_interface = List.mem(HInterface, herits);
			function build([]) return {
				c.cl_build = function []: False;
				try {
					set_heritance(ctx, c, herits, p);
					ClassInitializer.init_class(ctx, c, p, do_init, d.d_flags, d.d_data);
					c.cl_build = function []: True;
					List.iter(function (_, t): ignore(follow(t)), c.cl_params);
					True;
				} catch (e: T) {
					McOr(McArr(PaId(IdUid(Exit)), ExNil, ExSeq(ExSem(ExAss(<...>, <...>), ExSem(<...>, <...>)))), McArr(PaId(IdLid(exn)),
				ExNil, ExSeq(ExSem(ExAss(<...>, <...>), ExApp(<...>, <...>)))))								case Exit: c.cl_build = make_pass(ctx, build);
					delay(ctx, PTypeField, function []: ignore(c.cl_build([])));
					False;
				case exn: c.cl_build = function []: True;
					raise(exn);
				};
			};
			ctx.pass = PBuildClass;
			ctx.curclass = c;
			c.cl_build = make_pass(ctx, build);
			ctx.pass = PBuildModule;
			ctx.curclass = null_class;
			delay(ctx, PBuildClass, function []: ignore(c.cl_build([])));
			if ( && ( || ( = (ctx.com.platform, Java), = (ctx.com.platform, Cs)), !(c.cl_extern))) {
				delay(ctx, PTypeField, function []: var metas = check_strict_meta(ctx, c.cl_meta);
				if (<>(metas, [])) {
				c.cl_meta = @(metas, c.cl_meta);
				} else {
					[];
				};
				function run_field(cf) return {
					var metas = check_strict_meta(ctx, cf.cf_meta);
					if (<>(metas, [])) {
						cf.cf_meta = @(metas, cf.cf_meta);
					} else {
						[];
					};
					List.iter(run_field, cf.cf_overloads);
				};
				List.iter(run_field, c.cl_ordered_statics);
				List.iter(run_field, c.cl_ordered_fields);
				switch (c.cl_constructor) {
			case Some(f): run_field(f);
				case _: [];
				});
			} else {
				[];
			};
		case EEnum(d): var e = switch (get_type(d.d_name)) {
			case TEnumDecl(e): e;
			case _: assert False;
			};
			var ctx = { (ctx) with type_params = e.e_params };
			var h = try {
				Some(Hashtbl.find(ctx.g.type_patches, e.e_path));
			} catch (e: Not_found) {
				None;
			};
			check_global_metadata(ctx, function m: e.e_meta = ::(m, e.e_meta), e.e_module.m_path, e.e_path, None);
			switch (h) {
			case None: [];
			case Some(h, hcl): Hashtbl.iter(function _: function _: error("Field type patch not supported for enums", e.e_pos), h);
				e.e_meta = @(e.e_meta, hcl.tp_meta);
			};
			var constructs = ref(d.d_data);
			function get_constructs([]) return {
				List.map(function c: {
					() with cff_name = c.ec_name;
					cff_doc = c.ec_doc;
					cff_meta = c.ec_meta;
					cff_pos = c.ec_pos;
					cff_access = [];
					cff_kind = switch ((new Tuple(c.ec_args, c.ec_params))) {
					case ([], []): FVar(c.ec_type, None);
					case _: FFun({
							() with f_params = c.ec_params;
							f_type = c.ec_type;
							f_expr = None;
							f_args = List.map(function (n, o, t): (new Tuple(n, o, Some(t), None)), c.ec_args)
						});
					}
				}, constructs.val);
			};
			function init([]) return {
				List.iter(function f: f([]), context_init.val);
			};
			build_module_def(ctx, TEnumDecl(e), e.e_meta, get_constructs, init, function (e, p):
			switch (e) {
		case EVars(::((_, Some(CTAnonymous(fields)), None), [])): constructs.val = List.map(function f: var Tuple(args, params,
				t) = switch (f.cff_kind) {
			case FVar(t, None): (new Tuple([], [], t));
				case FFun({ f_params = pl; f_type = t; f_expr = None | Some(EBlock([]), _); f_args = al }): var al = List.map(function (n,
							o, t, _):
					switch (t) {
				case None: error("Missing function parameter type", f.cff_pos);
					case Some(t): (new Tuple(n, o, t));
					}, al);
					(new Tuple(al, pl, t));
				case _: error("Invalid enum constructor in @:build result", p);
				};
				{
					() with ec_name = f.cff_name;
					ec_doc = f.cff_doc;
					ec_meta = f.cff_meta;
					ec_pos = f.cff_pos;
					ec_args = args;
					ec_params = params;
					ec_type = t
				}, fields);
			case _: error("Enum build macro must return a single variable with anonymous object fields", p);
			});
			var et = TEnum(e, List.map(snd, e.e_params));
			var names = ref([]);
			var index = ref(0);
			var is_flat = ref(True);
			var fields = ref(PMap.empty);
			List.iter(function c: var p = c.ec_pos;
					  var params = ref([]);
					  params.val = type_type_params(enum_constructor = True, ctx, (new Tuple([], c.ec_name)), function []: params.val, c.ec_pos,
													c.ec_params);
					  var params = params.val;
					  var ctx = { (ctx) with type_params = @(params, ctx.type_params) };
			var rt = switch (c.ec_type) {
		case None: et;
		case Some(t): var t = load_complex_type(ctx, p, t);
				switch (follow(t)) {
				case TEnum(te, _) if (==(te, e)): [];
				case _: error("Explicit enum type must be of the same enum type", p);
				};
				t;
			};
			var t = switch (c.ec_args) {
		case []: rt;
			case l: is_flat.val = False;
				var pnames = ref(PMap.empty);
				TFun(List.map(function (s, opt, t):
				switch (t) {
			case CTPath({ tpackage = []; tname = Void }): error("Arguments of type Void are not allowed in enum constructors",
							c.ec_pos);
				case _: [];
				};
				if (PMap.mem(s, pnames.val)) {
				error( ^ ("Duplicate parameter '", ^ (s, ^ ("' in enum constructor ", c.ec_name))), p);
				} else {
					[];
				};
				pnames.val = PMap.add(s, [], pnames.val);
							 (new Tuple(s, opt, load_type_opt(opt = , ctx, p, Some(t)))), l), rt);
			};
			if (PMap.mem(c.ec_name, e.e_constrs)) {
			error( ^ ("Duplicate constructor ", c.ec_name), p);
			} else {
				[];
			};
			var f = { () with ef_name = c.ec_name;
					  ef_type = t;
					  ef_pos = p;
					  ef_doc = c.ec_doc;
					  ef_index = index.val;
					  ef_params = params;
					  ef_meta = c.ec_meta
					};
					var cf = { () with cf_name = f.ef_name;
							   cf_public = True;
							   cf_type = f.ef_type;
			cf_kind = switch (follow(f.ef_type)) {
		case TFun(_): Method(MethNormal);
			case _: Var({ () with v_read = AccNormal;
							  v_write = AccNo
							});
			};
			cf_pos = e.e_pos;
					 cf_doc = f.ef_doc;
					 cf_meta = no_meta;
					 cf_expr = None;
					 cf_params = f.ef_params;
					 cf_overloads = []
							 };
			e.e_constrs = PMap.add(f.ef_name, f, e.e_constrs);
						  fields.val = PMap.add(cf.cf_name, cf, fields.val);
						  incr(index);
						  names.val = ::(c.ec_name, names.val), constructs.val);
			e.e_names = List.rev(names.val);
			e.e_extern = e.e_extern;
			e.e_type.t_params = e.e_params;
			e.e_type.t_type = TAnon({ () with a_fields = fields.val;
									  a_status = ref(EnumStatics(e))
									});
			if (is_flat.val) {
				e.e_meta = ::((new Tuple(Meta.FlatEnum, [], e.e_pos)), e.e_meta);
			} else {
				[];
			};
			if ( && ( || ( = (ctx.com.platform, Java), = (ctx.com.platform, Cs)), !(e.e_extern))) {
				delay(ctx, PTypeField, function []: var metas = check_strict_meta(ctx, e.e_meta);
					  e.e_meta = @(metas, e.e_meta);
					  PMap.iter(function _: function ef: var metas = check_strict_meta(ctx, ef.ef_meta);
				if (<>(metas, [])) {
				ef.ef_meta = @(metas, ef.ef_meta);
				} else {
					[];
				}, e.e_constrs));
			} else {
				[];
			};
		case ETypedef(d): var t = switch (get_type(d.d_name)) {
			case TTypeDecl(t): t;
			case _: assert False;
			};
			check_global_metadata(ctx, function m: t.t_meta = ::(m, t.t_meta), t.t_module.m_path, t.t_path, None);
			var ctx = { (ctx) with type_params = t.t_params };
			var tt = load_complex_type(ctx, p, d.d_data);
			switch (d.d_data) {
			case CTExtend(_): [];
			case _: if ( == (t.t_type, follow(tt))) {
					error("Recursive typedef is not allowed", p);
				} else {
					[];
				};
			};
			switch (t.t_type) {
			case TMono(r): switch (r.val) {
				case None: r.val = Some(tt);
				case Some(_): assert False;
				};
			case _: assert False;
			};
			if ( && ( = (ctx.com.platform, Cs), <>(t.t_meta, []))) {
				delay(ctx, PTypeField, function []: var metas = check_strict_meta(ctx, t.t_meta);
				if (<>(metas, [])) {
				t.t_meta = @(metas, t.t_meta);
				} else {
					[];
				});
			} else {
				[];
			};
		case EAbstract(d): var a = switch (get_type(d.d_name)) {
			case TAbstractDecl(a): a;
			case _: assert False;
			};
			check_global_metadata(ctx, function m: a.a_meta = ::(m, a.a_meta), a.a_module.m_path, a.a_path, None);
			var ctx = { (ctx) with type_params = a.a_params };
			var is_type = ref(False);
			function load_type(t, from) return {
				var t = load_complex_type(ctx, p, t);
				var t = if (!(Meta.has(Meta.CoreType, a.a_meta))) {
					if (is_type.val) {
						var r = exc_protect(ctx, function r: r.val = function []: t;
						var at = monomorphs(a.a_params, a.a_this);
						try {
							if (from) {
								Type.unify(t, at);
							} else {
								Type.unify(at, t);
							};
						} catch (e: Unify_error(_)) {
							error("You can only declare from/to with compatible types", p);
						};
						t, "constraint");
						delay(ctx, PForce, function []: ignore(r.val([])));
						TLazy(r);
					} else {
						error("Missing underlying type declaration or @:coreType declaration", p);
					};
				} else {
					if (Meta.has(Meta.Callable, a.a_meta)) {
						error("@:coreType abstracts cannot be @:callable", p);
					} else {
						[];
					};
					t;
				};
				t;
			};
		List.iter(function case AFromType(t): a.a_from = ::(load_type(t, True), a.a_from);
					  case AToType(t): a.a_to = ::(load_type(t, False), a.a_to);
				case AIsType(t): if ( = (a.a_impl, None)) {
						error("Abstracts with underlying type must have an implementation", a.a_pos);
						} else {
							[];
						};
			if (Meta.has(Meta.CoreType, a.a_meta)) {
			error("@:coreType abstracts cannot have an underlying type", p);
			} else {
				[];
			};
			var at = load_complex_type(ctx, p, t);
			switch (at) {
		case TAbstract(a2, _) if (==(a, a2)): error("Abstract underlying type cannot be recursive", a.a_pos);
			case _: [];
			};
			a.a_this = at;
					   is_type.val = True;
				   case APrivAbstract: [], d.d_flags);
			if (!(is_type.val)) {
				if (Meta.has(Meta.CoreType, a.a_meta)) {
					a.a_this = TAbstract(a, List.map(snd, a.a_params));
				} else {
					error("Abstract is missing underlying type declaration", a.a_pos);
				};
			} else {
				[];
			};
		};
	};

	public static function type_module(ctx, m, file, ? : (is_extern = False), tdecls, p) return {
		var Tuple(m, decls, tdecls) = make_module(ctx, m, file, tdecls, p);
		if (is_extern) {
			m.m_extra.m_kind = MExtern;
		} else {
			[];
		};
		add_module(ctx, m, p);
		var ctx = { () with com = ctx.com;
					g = ctx.g;
					t = ctx.t;
					m = { () with curmod = m;
						  module_types = ctx.g.std.m_types;
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
					on_error = function ctx: function msg: function p: ctx.com.error(msg, p);
					macro_depth = ctx.macro_depth;
					curclass = null_class;
					curfield = null_field;
					tthis = ctx.tthis;
					ret = ctx.ret;
					locals = PMap.empty;
					type_params = [];
					curfun = FunStatic;
					untyped = False;
					in_super_call = False;
					in_macro = ctx.in_macro;
					in_display = False;
					in_loop = False;
					opened = [];
					vthis = None
				  };
		if ( != (ctx.g.std, null_module)) {
			add_dependency(m, ctx.g.std);
			ignore(load_core_type(ctx, "String"));
		} else {
			[];
		};
		List.iter(function d:
		switch (d) {
	case (TClassDecl(c), (EClass(d), p)): c.cl_params = type_type_params(ctx, c.cl_path, function []: c.cl_params, p,
					d.d_params);
		case (TEnumDecl(e), (EEnum(d), p)): e.e_params = type_type_params(ctx, e.e_path, function []: e.e_params, p, d.d_params);
		case (TTypeDecl(t), (ETypedef(d), p)): t.t_params = type_type_params(ctx, t.t_path, function []: t.t_params, p,
					d.d_params);
		case (TAbstractDecl(a), (EAbstract(d), p)): a.a_params = type_type_params(ctx, a.a_path, function []: a.a_params, p,
					d.d_params);
		case _: assert False;
		}, decls);
		var context_init = ref([]);
		function do_init([]) return {
			switch (context_init.val) {
			case []: [];
			case l: context_init.val = [];
				List.iter(function f: f([]), List.rev(l));
			};
		};
		List.iter(init_module_type(ctx, context_init, do_init), tdecls);
		m;
	};

	public static function resolve_module_file(com, m, remap, p) return {
		var forbid = ref(False);
		var file = ^ (switch (m) {
	case ([], name): name;
		case (::(x, l), name): var x = try {
				switch (PMap.find(x, com.package_rules)) {
				case Forbidden: forbid.val = True;
					x;
				case Directory(d): d;
				case Remap(d): remap.val = ::(d, l);
					d;
				};
			} catch (e: Not_found) {
				x;
			};
			^ (String.concat("/", ::(x, l)), ^ ("/", name));
		}, ".hx");
		var file = Common.find_file(com, file);
		var file = switch (String.lowercase(snd(m))) {
		case con | aux | prn | nul | com1 | com2 | com3 | lpt1 | lpt2 | lpt3 if (=(Sys.os_type, "Win32")):
			if ( > (try {
				Unix.stat(file).Unix.st_size;
				} catch (e: _) {
					0;
				}, 0)) {
				file;
			} else {
				raise(Not_found);
			};
		case _: file;
		};
		switch (fst(m)) {
		case ::(std, _): var file = Common.unique_full_path(file);
			if (List.exists(function path: ExtString.String.starts_with(file, try {
				Common.unique_full_path(path);
				} catch (e: _) {
					path;
				}), com.std_path)) {
				raise(Not_found);
			} else {
				[];
			};
		case _: [];
		};
		if (forbid.val) {
			var Tuple(_, decls) = parse_hook.val(com, file, p);
			function loop(decls) return {
				switch (decls) {
				case ::((EImport(_), _) | (EUsing(_), _), decls): loop(decls);
				case ::((EClass(d), _), _): d.d_meta;
				case ::((EEnum(d), _), _): d.d_meta;
				case ::((EAbstract(d), _), _): d.d_meta;
				case ::((ETypedef(d), _), _): d.d_meta;
				case []: [];
				};
			};
			var meta = loop(decls);
			if (!(Meta.has(Meta.NoPackageRestrict, meta))) {
				var x = switch (fst(m)) {
				case []: assert False;
				case ::(x, _): x;
				};
				raise(Forbid_package((new Tuple(x, m, p)), [], if (Common.defined(com, Define.Macro)) {
				"macro";
			} else {
				platform_name(com.platform);
				}));
			} else {
				[];
			};
		} else {
			[];
		};
		file;
	};

	public static function parse_module(ctx, m, p) return {
		var remap = ref(fst(m));
		var file = resolve_module_file(ctx.com, m, remap, p);
		var Tuple(pack, decls) = parse_hook.val(ctx.com, file, p);
		if (<>(pack, remap.val)) {
			function spack(m) return {
				if ( = (m, [])) {
					"<empty>";
				} else {
					String.concat(".", m);
				};
			};
			if ( == (p, Ast.null_pos)) {
				display_error(ctx, ^ ("Invalid commandline class : ", ^ (s_type_path(m), ^ (" should be ", s_type_path((new Tuple(pack,
									  snd(m))))))), p);
			} else {
				display_error(ctx, ^ ("Invalid package : ", ^ (spack(fst(m)), ^ (" should be ", spack(pack)))), p);
			};
		} else {
			[];
		};
		(new Tuple(file, if (<>(remap.val, fst(m))) {
		List.rev(List.fold_left(function acc: function (t, p): 	function build(f, d) return {
			var priv = List.mem(f, d.d_flags);
				::((new Tuple(ETypedef({
					() with d_name = d.d_name;
					d_doc = None;
					d_meta = [];
					d_params = d.d_params;
					d_flags = if (priv) {
						::(EPrivate, []);
					} else {
						[];
					};
					d_data = CTPath(if (priv) {
					{
						() with tpackage = [];
							tname = "Dynamic";
							tparams = [];
							tsub = None
						};
					} else {
						{
							() with tpackage = remap.val;
							tname = d.d_name;
							tparams = List.map(function tp: TPType(CTPath({
								() with tpackage = [];
								tname = tp.tp_name;
								tparams = [];
								tsub = None
							})), d.d_params);
							tsub = None
						};
					})
				}), p)), acc);
			};
			switch (t) {
		case EClass(d): build(HPrivate, d);
			case EEnum(d): build(EPrivate, d);
			case ETypedef(d): build(EPrivate, d);
			case EAbstract(d): build(APrivAbstract, d);
			case EImport(_) | EUsing(_): acc;
			}, ::((new Tuple(EImport(List.map(function s: (new Tuple(s, null_pos)), @(remap.val, ::(snd(m), []))), INormal),
							 null_pos)), []), decls));
		} else {
			decls;
		}));
	};

	public static function load_module(ctx, m, p) return {
		var m2 = try {
			Hashtbl.find(ctx.g.modules, m);
		} catch (e: Not_found) {
			switch (type_module_hook.val(ctx, m, p)) {
			case Some(m): m;
			case None: var is_extern = ref(False);
				var Tuple(file, decls) = try {
					parse_module(ctx, m, p);
				} catch (e: Not_found) {
					function loop(match) return switch (match) {
					case []: raise(Error(Module_not_found(m), p));
					case ::(load, l): switch (load(m, p)) {
						case None: loop(l);
						case Some(file, (_, a)): (new Tuple(file, a));
						};
					};
					is_extern.val = True;
					loop(ctx.com.load_extern_type);
				};
				var is_extern = is_extern.val;
				try {
					type_module(ctx, m, file, is_extern = , decls, p);
				} catch (e: Forbid_package(inf)(pl)(pf)) {
					raise(Forbid_package(inf, ::(p, pl), pf));
				};
			};
		};
		add_dependency(ctx.m.curmod, m2);
		if ( = (ctx.pass, PTypeField)) {
			flush_pass(ctx, PBuildClass, "load_module");
		} else {
			[];
		};
		m2;
	};

	public static function __init__() {
		type_function_params_rec.val = type_function_params;
	}
}
;
