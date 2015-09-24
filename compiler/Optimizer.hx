import Ast;
import Type;
import Common;
import Typecore;

typedef In_local = {
	i_var : Tvar,
	i_subst : Tvar,
	i_captured : Bool,
	i_write : Bool,
	i_read : Int,
	i_force_temp : Bool
};

enum Inline_kind {
	IKCtor(value: TfuncTclass_fieldTclassList<T>List<Texpr>List<Texpr>);
	IKArray(value: List<Texpr>T);
	IKStructure(value: List<Tuple<String, Texpr>>);
	IKNone;
};

class /*exception*/ Return {

};

typedef Compl_locals = {
	r : PMap<String, Tuple<Option<Complex_type>, Option<Tuple<Int, Ast.Expr, Compl_locals>>>>
};

class Optimizer {
	public static function has_side_effect(e) return {
		function loop(e) return {
			switch (e.eexpr) {
			case TConst(_) | TLocal(_) | TTypeExpr(_) | TFunction(_): [];
			case TCall({ eexpr = TField(_, FStatic({ cl_path = ([], Std) }, { cf_name = string })) }, args): Type.iter(loop, e);
			case TNew(_) | TCall(_) | TBinop(OpAssignOp(_) | OpAssign, _, _) | TUnop(Increment | Decrement, _, _): raise(Exit);
			case TReturn(_) | TBreak | TContinue | TThrow(_) | TCast(_, Some(_)): raise(Exit);
			case TArray(_) | TEnumParameter(_) | TCast(_, None) | TBinop(_) | TUnop(_) | TParenthesis(_) | TMeta(_) | TWhile(_) | TFor(_) | TField(_) | TIf(_) | TTry(_) | TSwitch(_) | TArrayDecl(_) | TBlock(_) | TObjectDecl(_) | TVar(_)
					: Type.iter(loop, e);
			};
		};
		try {
			loop(e);
			False;
		} catch (e: Exit) {
			True;
		};
	};

	public static function is_exhaustive(e1) return {
		switch (e1.eexpr) {
		case TMeta((Meta.Exhaustive, _, _), _): True;
		case TMeta(_, e1) | TParenthesis(e1): is_exhaustive(e1);
		case _: False;
		};
	};

	public static function mk_untyped_call(name, p, params) return {
		{
			() with eexpr = TCall({
				() with eexpr = TLocal(alloc_unbound_var(name, t_dynamic));
				etype = t_dynamic;
				epos = p
			}, params);
			etype = t_dynamic;
			epos = p
		};
	};

	public static function api_inline2(com, c, field, params, p) return {
		switch ((new Tuple(c.cl_path, field, params))) {
		case (([], Type), enumIndex, ::({ eexpr = TField(_, FEnum(en, f)) }, [])):
			switch (com.platform) {
			case Cs if (&&(en.e_extern, !(Meta.has(Meta.HxGen, en.e_meta)))): None;
			case _: Some(mk(TConst(TInt(Int32.of_int(f.ef_index))), com.basic.tint, p));
			};
		case (([], Type), enumIndex, ::({ eexpr = TCall({ eexpr = TField(_, FEnum(en, f)) }, pl) }, [])) if (List.for_all(function e
						: !(has_side_effect(e)), pl)):
			switch (com.platform) {
			case Cs if (&&(en.e_extern, !(Meta.has(Meta.HxGen, en.e_meta)))): None;
			case _: Some(mk(TConst(TInt(Int32.of_int(f.ef_index))), com.basic.tint, p));
			};
		case (([], Std), int, ::({ eexpr = TConst(TInt(_)) } = e, [])): Some({ (e) with epos = p });
		case (([], String), fromCharCode, ::({ eexpr = TConst(TInt(i)) }, [])) if (&&(>(i, ), <(i, ))): Some(mk(TConst(TString(
						String.make(1, char_of_int(Int32.to_int(i))))), com.basic.tstring, p));
		case (([], Std), string, ::({ eexpr = TConst(c) } = e, [])):
			switch (c) {
			case TString(s): Some({ (e) with epos = p });
			case TInt(i): Some({ () with eexpr = TConst(TString(Int32.to_string(i)));
									 epos = p;
									 etype = com.basic.tstring
								   });
			case TBool(b): Some({ () with eexpr = TConst(TString(if (b) {
				"true";
			} else {
				"false";
			}));
			epos = p;
			etype = com.basic.tstring
								});
			case _: None;
			};
		case (([], Std), string, ::({ eexpr = TIf(_, { eexpr = TConst(TString(_)) }, Some({ eexpr = TConst(TString(_)) })) } = e, []))
				: Some(e);
		case (([], Std), string, ::({ eexpr = TLocal(v) | TField({ eexpr = TLocal(v) }, _) } = ev, [])) if (&&(||(=(com.platform, Js), =(com.platform, Flash)), !(Meta.has(Meta.CompilerGenerated, v.v_meta))))
					: var pos = ev.epos;
			function stringv([]) return {
				var to_str = mk(TBinop(Ast.OpAdd, mk(TConst(TString("")), com.basic.tstring, pos), ev), com.basic.tstring, pos);
				if ( || ( = (com.platform, Js), is_nullable(ev.etype))) {
					var chk_null = mk(TBinop(Ast.OpEq, ev, mk(TConst(TNull), t_dynamic, pos)), com.basic.tbool, pos);
					mk(TIf(chk_null, mk(TConst(TString("null")), com.basic.tstring, pos), Some(to_str)), com.basic.tstring, pos);
				} else {
					to_str;
				};
			};
			switch (follow(ev.etype)) {
			case TInst({ cl_path = ([], String) }, []): Some(stringv([]));
			case TAbstract({ a_path = ([], Float) }, []): Some(stringv([]));
			case TAbstract({ a_path = ([], Int) }, []): Some(stringv([]));
			case TAbstract({ a_path = ([], UInt) }, []): Some(stringv([]));
			case TAbstract({ a_path = ([], Bool) }, []): Some(stringv([]));
			case _: None;
			};
		case (([], Std), int, ::({ eexpr = TConst(TFloat(f)) }, [])): var f = float_of_string(f);
			switch (classify_float(f)) {
			case FP_infinite | FP_nan: None;
			case _ if (||(<=(f, -.(Int32.to_float(Int32.min_int), 1.)), >=(f, +.(Int32.to_float(Int32.max_int), 1.)))): None;
			case _: Some({ () with eexpr = TConst(TInt(Int32.of_float(f)));
							   etype = com.basic.tint;
							   epos = p
							 });
			};
		case (([], Math), ceil, ::({ eexpr = TConst(TFloat(f)) }, [])): var f = float_of_string(f);
			switch (classify_float(f)) {
			case FP_infinite | FP_nan: None;
			case _ if (||(<=(f, -.(Int32.to_float(Int32.min_int), 1.)), >=(f, Int32.to_float(Int32.max_int)))): None;
			case _: Some({ () with eexpr = TConst(TInt(Int32.of_float(ceil(f))));
							   etype = com.basic.tint;
							   epos = p
							 });
			};
		case (([], Math), floor, ::({ eexpr = TConst(TFloat(f)) }, [])): var f = float_of_string(f);
			switch (classify_float(f)) {
			case FP_infinite | FP_nan: None;
			case _ if (||(<=(f, Int32.to_float(Int32.min_int)), >=(f, +.(Int32.to_float(Int32.max_int), 1.)))): None;
			case _: Some({ () with eexpr = TConst(TInt(Int32.of_float(floor(f))));
							   etype = com.basic.tint;
							   epos = p
							 });
			};
		case ((::(cs, []), Lib), fixed | checked | unsafe, ::(e, [])): Some(mk_untyped_call( ^ ("__", ^ (field, "__")), p,
					::(e, [])));
		case ((::(cs, []), Lib), lock, ::(obj, ::(block, []))): Some(mk_untyped_call("__lock__", p, ::(obj,
					::(mk_block(block), []))));
		case ((::(java, []), Lib), lock, ::(obj, ::(block, []))): Some(mk_untyped_call("__lock__", p, ::(obj,
					::(mk_block(block), []))));
		case _: None;
		};
	};

	public static function api_inline(ctx, c, field, params, p) return {
		switch ((new Tuple(c.cl_path, field, params))) {
		case (([], Std), is, ::(o, ::(t, []))) | ((::(js, []), Boot), __instanceof, ::(o, ::(t, []))) if (=(ctx.com.platform, Js)):
			function mk_local(ctx, n, t, pos) return {
				mk(TLocal(try {
					PMap.find(n, ctx.locals);
				} catch (e: _) {
					var v = add_local(ctx, n, t);
					v.v_meta = ::((new Tuple(Meta.Unbound, [], p)), []);
					v;
				}), t, pos);
			};
			var tstring = ctx.com.basic.tstring;
			var tbool = ctx.com.basic.tbool;
			var tint = ctx.com.basic.tint;
			function is_trivial(e) return {
				switch (e.eexpr) {
				case TConst(_) | TLocal(_): True;
				case _: False;
				};
			};
			function typeof(t) return {
				var tof = mk_local(ctx, "__typeof__", tfun(::(o.etype, []), tstring), p);
				var tof = mk(TCall(tof, ::(o, [])), tstring, p);
				mk(TBinop(Ast.OpEq, tof, mk(TConst(TString(t)), tstring, p)), tbool, p);
			};
			switch (t.eexpr) {
			case TTypeExpr(TClassDecl({ cl_path = ([], String) })): Some(typeof("string"));
			case TTypeExpr(TAbstractDecl({ a_path = ([], Bool) })): Some(typeof("boolean"));
			case TTypeExpr(TAbstractDecl({ a_path = ([], Float) })): Some(typeof("number"));
			case TTypeExpr(TAbstractDecl({ a_path = ([], Int) })) if (is_trivial(o)): var teq = mk_local(ctx, "__strict_eq__",
						tfun(::(tint, ::(tint, [])), tbool), p);
				var lhs = mk(TBinop(Ast.OpOr, o, mk(TConst(TInt(Int32.zero)), tint, p)), tint, p);
				Some(mk(TCall(teq, ::(lhs, ::(o, []))), tbool, p));
			case TTypeExpr(TClassDecl({ cl_path = ([], Array) })): var iof = mk_local(ctx, "__instanceof__", tfun(::(o.etype,
						::(t.etype, [])), tbool), p);
				var iof = mk(TCall(iof, ::(o, ::(t, []))), tbool, p);
				var enum = mk(TField(o, FDynamic("__enum__")), mk_mono([]), p);
				var null = mk(TConst(TNull), mk_mono([]), p);
				var not_enum = mk(TBinop(Ast.OpEq, enum, null), tbool, p);
				Some(mk(TBinop(Ast.OpBoolAnd, iof, not_enum), tbool, p));
			case _: None;
			};
		case ((::(cs | java, []), Lib), nativeArray, ::({ eexpr = TArrayDecl(args) } = edecl, ::(_, []))) | ((::(haxe, ::(ds, ::(_Vector, []))), Vector_Impl_), fromArrayCopy, ::({ eexpr = TArrayDecl(args) } = edecl, []))
				:
			try {
				var platf = switch (ctx.com.platform) {
				case Cs: "cs";
				case Java: "java";
				case _: raise(Exit);
				};
				var mpath = if ( = (field, "fromArrayCopy")) {
					(new Tuple(::("haxe", ::("ds", [])), "Vector"));
				} else {
					(new Tuple(::(platf, []), "NativeArray"));
				};
				var m = ctx.g.do_load_module(ctx, mpath, null_pos);
			var main = List.find(function case TClassDecl(_) | TAbstractDecl(_): True;
									 case _: False, m.m_types);
				var t = switch ((new Tuple(follow(edecl.etype), main))) {
				case (TInst({ cl_path = ([], Array) }, ::(t, [])), TClassDecl(cl)): TInst(cl, ::(t, []));
				case (TInst({ cl_path = ([], Array) }, ::(t, [])), TAbstractDecl(a)): TAbstract(a, ::(t, []));
				case _: assert False;
				};
				Some({ (mk_untyped_call("__array__", p, args)) with etype = t });
			} catch (e: Exit) {
				None;
			};
		case _: api_inline2(ctx.com, c, field, params, p);
		};
	};

	public static function inline_default_config(cf, t) return {
		function get_params(c, pl) return {
			switch (c.cl_super) {
			case None: (new Tuple(c.cl_params, pl));
			case Some(csup, spl): var spl = switch (apply_params(c.cl_params, pl, TInst(csup, spl))) {
				case TInst(_, pl): pl;
				case _: assert False;
				};
				var Tuple(ct, cpl) = get_params(csup, spl);
				(new Tuple(@(c.cl_params, ct), @(pl, cpl)));
			};
		};
		var tparams = switch (follow(t)) {
		case TInst(c, pl): get_params(c, pl);
		case _: (new Tuple([], []));
		};
		var pmonos = List.map(function _: mk_mono([]), cf.cf_params);
		var tmonos = @(snd(tparams), pmonos);
		var tparams = @(fst(tparams), cf.cf_params);
		(new Tuple(<>(tparams, []), apply_params(tparams, tmonos)));
	};

	public static function type_inline(ctx, cf, f, ethis, params, tret, config, p, ? : (self_calling_closure = False),
									   force) return {
		try {
			var cl = switch (follow(ethis.etype)) {
			case TInst(c, _): c;
			case TAnon(a): switch (a.a_status.val) {
				case Statics(c): c;
				case _: raise(Exit);
				};
			case _: raise(Exit);
			};
			switch (api_inline(ctx, cl, cf.cf_name, params, p)) {
			case None: raise(Exit);
			case Some(e): Some(e);
			};
		} catch (e: Exit) {
			var Tuple(has_params, map_type) = switch (config) {
			case Some(config): config;
			case None: inline_default_config(cf, ethis.etype);
			};
			var locals = Hashtbl.create(0);
			function local(v) return {
				try {
					Hashtbl.find(locals, v.v_id);
				} catch (e: Not_found) {
					var v_ = alloc_var(v.v_name, v.v_type);
					if (Meta.has(Meta.Unbound, v.v_meta)) {
						v_.v_meta = ::((new Tuple(Meta.Unbound, [], p)), []);
					} else {
						[];
					};
					var i = { () with i_var = v;
							  i_subst = v_;
							  i_captured = False;
							  i_write = False;
							  i_force_temp = False;
							  i_read = 0
							};
					i.i_subst.v_meta = v.v_meta;
					Hashtbl.add(locals, v.v_id, i);
					Hashtbl.add(locals, i.i_subst.v_id, i);
					i;
				};
			};
			var in_local_fun = ref(False);
			function read_local(v) return {
				var l = try {
					Hashtbl.find(locals, v.v_id);
				} catch (e: Not_found) {
					if (has_meta(Meta.Unbound, v.v_meta)) {
						local(v);
					} else {
						{
							() with i_var = v;
							i_subst = v;
							i_captured = False;
							i_write = False;
							i_force_temp = False;
							i_read = 0
						};
					};
				};
				if (in_local_fun.val) {
					l.i_captured = True;
				} else {
					[];
				};
				l;
			};
			function loop(pl, al, first) return {
				switch ((new Tuple(pl, al))) {
				case (_, []): [];
				case (::(e, pl), ::((v, opt), al)): if ( && (ctx.com.config.pf_static, && (!(is_nullable(v.v_type)), is_null(e.etype)))) {
						local(v).i_force_temp = True;
					} else {
						[];
					};
					if ( && ( != (v.v_type, t_dynamic), == (follow(e.etype), t_dynamic))) {
						local(v).i_write = True;
					} else {
						[];
					};
					::(switch ((new Tuple(e.eexpr, opt))) {
				case (TConst(TNull), Some(c)): mk(TConst(c), v.v_type, e.epos);
					case _: e;
					}, loop(pl, al, False));
				case ([], ::((v, opt), al)): ::(mk(TConst(switch (opt) {
				case None: TNull;
				case Some(c): c;
					}), v.v_type, p), loop([], al, False));
				};
			};
			var ethis = switch (ethis.eexpr) {
			case TConst(TSuper): {
				(ethis) with eexpr = TConst(TThis)
			};
			case _: ethis;
			};
			var vthis = alloc_var("_this", ethis.etype);
			var inlined_vars = List.map2(function e: function (v, _): var l = local(v);
			if (has_side_effect(e)) {
			l.i_force_temp = True;
		} else {
			[];
			};
			(new Tuple(l, e)), ::(ethis, loop(params, f.tf_args, True)), ::((new Tuple(vthis, None)), f.tf_args));
			var inlined_vars = List.rev(inlined_vars);
			function opt(f) return {
			case None: None;
			case Some(e): Some(f(e));
			};
			var has_vars = ref(False);
			var in_loop = ref(False);
			var cancel_inlining = ref(False);
			var has_return_value = ref(False);
			var ret_val = switch (follow(f.tf_type)) {
			case TAbstract({ a_path = ([], Void) }, []): False;
			case _: True;
			};
			var map_pos = if (self_calling_closure) {
				function e: e;
			} else {
				function e: {
					(e) with epos = p
				};
			};
			function map(term, e) return {
				var po = e.epos;
				var e = map_pos(e);
				switch (e.eexpr) {
				case TLocal(v): var l = read_local(v);
					l.i_read = +(l.i_read, if (in_loop.val) {
					2;
				} else {
					1;
				});
					if ( = (v.v_name, "__dollar__delay_call")) {
						cancel_inlining.val = True;
					} else {
						[];
					};
					var e = { (e) with eexpr = TLocal(l.i_subst) };
					if (Meta.has(Meta.This, v.v_meta)) {
						mk(TCast(e, None), v.v_type, e.epos);
					} else {
						e;
					};
				case TConst(TThis): var l = read_local(vthis);
					l.i_read = +(l.i_read, if (in_loop.val) {
					2;
				} else {
					1;
				});
					{
						(e) with eexpr = TLocal(l.i_subst)
					};
				case TVar(v, eo): has_vars.val = True;
					{
						(e) with eexpr = TVar(local(v).i_subst, opt(map(False), eo))
					};
				case TReturn(eo) if (!(in_local_fun.val)):
					if (!(term)) {
						error("Cannot inline a not final return", po);
					} else {
						[];
					};
					switch (eo) {
					case None: mk(TConst(TNull), f.tf_type, p);
					case Some(e): has_return_value.val = True;
						map(term, e);
					};
				case TFor(v, e1, e2): var i = local(v);
					var e1 = map(False, e1);
					var old = in_loop.val;
					in_loop.val = True;
					var e2 = map(False, e2);
					in_loop.val = old;
					{
						(e) with eexpr = TFor(i.i_subst, e1, e2)
					};
				case TWhile(cond, eloop, flag): var cond = map(False, cond);
					var old = in_loop.val;
					in_loop.val = True;
					var eloop = map(False, eloop);
					in_loop.val = old;
					{
						(e) with eexpr = TWhile(cond, eloop, flag)
					};
				case TSwitch(e1, cases, def) if (term): var term = && (term, || (<>(def, None), is_exhaustive(e1)));
					var cases = List.map(function (el, e): var el = List.map(map(False), el);
										 (new Tuple(el, map(term, e))), cases);
					var def = opt(map(term), def);
					{
						(e) with eexpr = TSwitch(map(False, e1), cases, def);
						etype = if (ret_val) {
							unify_min(ctx, @(List.map(snd, cases), switch (def) {
						case None: [];
							case Some(e): ::(e, []);
							}));
						} else {
							e.etype;
						}
					};
				case TTry(e1, catches): {
					(e) with eexpr = TTry(map(term, e1), List.map(function (v, e): var lv = local(v).i_subst;
										  var e = map(term, e);
										  (new Tuple(lv, e)), catches));
					etype = if ( && (term, ret_val)) {
						unify_min(ctx, ::(e1, List.map(snd, catches)));
					} else {
						e.etype;
					}
				};
				case TBlock(l): var old = save_locals(ctx);
					var t = ref(e.etype);
					function has_term_return(e) return {
						function loop(e) return {
							var r = switch (e.eexpr) {
							case TReturn(_): True;
							case TIf(_, _, None) | TSwitch(_, _, None) | TFor(_) | TWhile(_, _, NormalWhile): False;
							case TTry(a, catches): List.for_all(has_term_return, ::(a, List.map(snd, catches)));
							case TIf(cond, a, Some(b)): || (has_term_return(cond), && (has_term_return(a), has_term_return(b)));
							case TSwitch(cond, cases, Some(def)):
								|| (has_term_return(cond), List.for_all(has_term_return, ::(def, List.map(snd, cases))));
							case TBinop(OpBoolAnd, a, b): && (has_term_return(a), has_term_return(b));
							case _: Type.iter(loop, e);
								False;
							};
							if (r) {
								raise(Exit);
							} else {
								[];
							};
						};
						try {
							loop(e);
							False;
						} catch (e: Exit) {
							True;
						};
					};
					function loop(match) return switch (match) {
					case [] if (term): t.val = mk_mono([]);
						::(mk(TConst(TNull), t.val, p), []);
					case []: [];
					case ::(e, []): var e = map(term, e);
						if (term) {
							t.val = e.etype;
						} else {
							[];
						};
						::(e, []);
					case ::( {
										 eexpr = TIf(cond, e1, None)
									 } = e, l) if ( && (term, has_term_return(e1))): loop(::({ (e) with eexpr = TIf(cond, e1, Some(mk(TBlock(l), e.etype, e.epos)));
						epos = punion(e.epos, switch (List.rev(l)) {
					case ::(e, _): e.epos;
						case []: assert False;
						})
																						 }, []));
					case ::(e, l): var e = map(False, e);
						::(e, loop(l));
					};
					var l = loop(l);
					old([]);
					{
						(e) with eexpr = TBlock(l);
						etype = t.val
					};
				case TIf(econd, eif, Some(eelse)) if (term): var econd = map(False, econd);
					var eif = map(term, eif);
					var eelse = map(term, eelse);
					{
						(e) with eexpr = TIf(econd, eif, Some(eelse));
						etype = if (ret_val) {
							unify_min(ctx, ::(eif, ::(eelse, [])));
						} else {
							e.etype;
						}
					};
				case TParenthesis(e1): var e1 = map(term, e1);
					mk(TParenthesis(e1), e1.etype, e.epos);
				case TUnop(Increment | Decrement = op, flag, { eexpr = TLocal(v) } = e1): var l = read_local(v);
					l.i_write = True;
					{
						(e) with eexpr = TUnop(op, flag, { (e1) with eexpr = TLocal(l.i_subst) })
					};
				case TBinop(OpAssign | OpAssignOp(_) = op, { eexpr = TLocal(v) } = e1, e2): var l = read_local(v);
					l.i_write = True;
					var e2 = map(False, e2);
					{
						(e) with eexpr = TBinop(op, { (e1) with eexpr = TLocal(l.i_subst) }, e2)
					};
				case TObjectDecl(fl): var fl = List.map(function (s, e): (new Tuple(s, map(False, e))), fl);
					switch (follow(e.etype)) {
					case TAnon(an) if (switch (an.a_status.val) {
							case Const: True;
							case _: False;
							}): {
							(e) with eexpr = TObjectDecl(fl);
							etype = TAnon({ (an) with a_status = ref(Closed) })
						};
					case _: {
							(e) with eexpr = TObjectDecl(fl)
						};
					};
				case TFunction(f): switch (f.tf_args) {
					case []: [];
					case _: has_vars.val = True;
					};
					var old = save_locals(ctx);
					var old_fun = in_local_fun.val;
					var args = List.map(function (v, c): (new Tuple(local(v).i_subst, c)), f.tf_args);
					in_local_fun.val = True;
					var expr = map(False, f.tf_expr);
					in_local_fun.val = old_fun;
					old([]);
					{
						(e) with eexpr = TFunction( { () with tf_args = args;
													  tf_expr = expr;
													  tf_type = f.tf_type
													})
					};
				case TConst(TSuper): error("Cannot inline function containing super", po);
				case TMeta(m, e1): var e1 = map(term, e1);
					{
						(e) with eexpr = TMeta(m, e1)
					};
				case _: Type.map_expr(map(False), e);
				};
			};
			var e = map(True, f.tf_expr);
			var subst = ref(PMap.empty);
			function is_constant(e) return {
				function loop(e) return {
					switch (e.eexpr) {
					case TLocal(_) | TConst(TThis): raise(Exit);
					case TField(_, FEnum(_)) | TTypeExpr(_) | TConst(_): [];
					case _: Type.iter(loop, e);
					};
				};
				try {
					loop(e);
					True;
				} catch (e: Exit) {
					False;
				};
			};
			function is_writable(e) return {
				switch (e.eexpr) {
				case TField(_) | TEnumParameter(_) | TLocal(_) | TArray(_): True;
				case _: False;
				};
			};
			var force = ref(force);
			var vars = List.fold_left(function acc: function (i, e): var flag = && (!(i.i_force_temp), switch (e.eexpr) {
		case TLocal(v) if (Meta.has(Meta.This, v.v_meta)): True;
			case TLocal(_) | TConst(_): !(i.i_write);
			case TFunction(_): if (i.i_write) {
					error("Cannot modify a closure parameter inside inline method", p);
				} else {
					[];
				};
				True;
			case _: && (!(i.i_write), <= (i.i_read, 1));
			});
			var flag = && (flag, || (!(i.i_captured), is_constant(e)));
			if ( && (i.i_write, Meta.has(Meta.This, i.i_var.v_meta))) {
			force.val = True;
		} else {
			[];
			};
			var flag = if ( && (!(flag), && (Meta.has(Meta.This, i.i_var.v_meta), i.i_write))) {
			if (!(is_writable(e))) {
					error("Cannot modify the abstract value, store it into a local first", p);
				} else {
					[];
				};
				True;
			} else {
				flag;
			};
			if (flag) {
			subst.val = PMap.add(i.i_subst.v_id, e, subst.val);
				acc;
			} else {
				::((new Tuple(i.i_subst, Some(e))), acc);
			}, [], inlined_vars);
			var subst = subst.val;
			function inline_params(e) return {
				switch (e.eexpr) {
				case TLocal(v): try {
						PMap.find(v.v_id, subst);
					} catch (e: Not_found) {
						e;
					};
				case _: Type.map_expr(inline_params, e);
				};
			};
			var e = if (PMap.is_empty(subst)) {
				e;
			} else {
				inline_params(e);
			};
			var init = switch (vars) {
			case []: None;
			case l: Some(l);
			};
			if ( || (cancel_inlining.val, && (!(Common.defined(ctx.com, Define.Analyzer)), && (Common.platform(ctx.com, Js),
											  && (!(force.val), || (<>(init, None), has_vars.val)))))) {
				None;
			} else {
				function wrap(e) return {
					var etype = if (has_params) {
						map_type(e.etype);
					} else {
						e.etype;
					};
					try {
						switch (follow(e.etype)) {
						case TMono(_) | TInst({ cl_kind = KTypeParameter(_) }, _):
							switch (follow(tret)) {
							case TAbstract({ a_path = ([], Void) }, _): e;
							case _: raise(Unify_error([]));
							};
						case _: type_eq(if (ctx.com.config.pf_static) {
							EqDoNotFollowNull;
						} else {
							EqStrict;
						}, etype, tret);
							e;
						};
					} catch (e: Unify_error(_)) {
						mk(TCast(e, None), tret, e.epos);
					};
				};
				var e = switch ((new Tuple(e.eexpr, init))) {
				case (_, None) if (!(has_return_value.val)): {
					(e) with etype = tret
				};
				case (TBlock(::(e, [])), None): wrap(e);
				case (_, None): wrap(e);
				case (TBlock(l), Some(vl)): var el_v = List.map(function (v, eo): mk(TVar(v, eo), ctx.t.tvoid, e.epos), vl);
					mk(TBlock(@(el_v, l)), tret, e.epos);
				case (_, Some(vl)): var el_v = List.map(function (v, eo): mk(TVar(v, eo), ctx.t.tvoid, e.epos), vl);
					mk(TBlock(@(el_v, ::(e, []))), tret, e.epos);
				};
				function inline_meta(e, meta) return {
					switch (meta) {
					case (Meta.Deprecated, _, _): mk(TMeta(meta, e), e.etype, e.epos);
					case _: e;
					};
				};
				var e = List.fold_left(inline_meta, e, cf.cf_meta);
				if (!(has_params)) {
					Some(e);
				} else {
					var mt = map_type(cf.cf_type);
					function unify_func([]) return {
						unify_raise(ctx, mt, TFun(List.map(function e: (new Tuple("", False, e.etype)), params), tret), p);
					};
					switch (follow(ethis.etype)) {
					case TAnon(a): switch (a.a_status.val) {
						case Statics({ cl_kind = KAbstractImpl(a) }) if (Meta.has(Meta.Impl, cf.cf_meta)):
							if (<>(cf.cf_name, "_new")) {
								var tb = TFun(::((new Tuple("", False, map_type(a.a_this))), List.map(function e: (new Tuple("", False, e.etype)),
												 List.tl(params))), tret);
								unify_raise(ctx, mt, tb, p);
							} else {
								[];
							};
						case _: unify_func([]);
						};
					case _: unify_func([]);
					};
					var vars = Hashtbl.create(0);
					function map_var(v) return {
						if (!(Hashtbl.mem(vars, v.v_id))) {
							Hashtbl.add(vars, v.v_id, []);
							v.v_type = map_type(v.v_type);
						} else {
							[];
						};
						v;
					};
					function map_expr_type(e) return {
						Type.map_expr_type(map_expr_type, map_type, map_var, e);
					};
					Some(map_expr_type(e));
				};
			};
		};
	};

	public static function optimize_for_loop(ctx, Tuple(i, pi), e1, e2, p) return {
		var t_void = ctx.t.tvoid;
		var t_int = ctx.t.tint;
		function lblock(el) return {
			Some(mk(TBlock(el), t_void, p));
		};
		function mk_field(e, n) return {
			TField(e, try {
				quick_field(e.etype, n);
			} catch (e: Not_found) {
				assert False;
			});
		};
		function gen_int_iter(pt, f_get, f_length) return {
			var i = add_local(ctx, i, pt);
			var index = gen_local(ctx, t_int);
			var Tuple(arr, avars) = switch (e1.eexpr) {
			case TLocal(_): (new Tuple(e1, None));
			case _: var atmp = gen_local(ctx, e1.etype);
				(new Tuple(mk(TLocal(atmp), e1.etype, e1.epos), Some(atmp, Some(e1))));
			};
			var iexpr = mk(TLocal(index), t_int, p);
			var e2 = type_expr(ctx, e2, NoValue);
			var aget = mk(TVar(i, Some(f_get(arr, iexpr, pt, p))), t_void, pi);
			var incr = mk(TUnop(Increment, Prefix, iexpr), t_int, p);
			var block = switch (e2.eexpr) {
			case TBlock(el): mk(TBlock(::(aget, ::(incr, el))), t_void, e2.epos);
			case _: mk(TBlock(::(aget, ::(incr, ::(e2, [])))), t_void, p);
			};
			var ivar = Some(mk(TConst(TInt()), t_int, p));
			var elength = f_length(arr, p);
			var el = ::(mk(TWhile(mk(TBinop(OpLt, iexpr, elength), ctx.t.tbool, p), block, NormalWhile), t_void, p), []);
			var el = switch (avars) {
			case None: el;
			case Some(v, eo): ::(mk(TVar(v, eo), t_void, p), el);
			};
			var el = ::(mk(TVar(index, ivar), t_void, p), el);
			lblock(el);
		};
		function get_next_array_element(arr, iexpr, pt, p) return {
			mk(TArray(arr, iexpr), pt, p);
		};
		function get_array_length(arr, p) return {
			mk(mk_field(arr, "length"), ctx.com.basic.tint, p);
		};
		switch ((new Tuple(e1.eexpr, follow(e1.etype)))) {
		case (TNew({ cl_path = ([], IntIterator) }, [], ::(i1, ::(i2, []))), _): var max = switch ((new Tuple(i1.eexpr,
			i2.eexpr))) {
			case (TConst(TInt(a)), TConst(TInt(b))) if (<(Int32.compare(b, a), 0)): error("Range operator can't iterate backwards", p);
			case (_, TConst(_)) | (_, TLocal(_)): None;
			case _: Some(gen_local(ctx, t_int));
			};
			var tmp = gen_local(ctx, t_int);
			var i = add_local(ctx, i, t_int);
			function check(e) return {
				switch (e.eexpr) {
				case TBinop(OpAssign, { eexpr = TLocal(l) }, _) | TBinop(OpAssignOp(_), { eexpr = TLocal(l) }, _) | TUnop(Increment, _, { eexpr = TLocal(l) }) | TUnop(Decrement, _, { eexpr = TLocal(l) }) if (==(l, i))
						: error("Loop variable cannot be modified", e.epos);
				case _: Type.iter(check, e);
				};
			};
			var e2 = type_expr(ctx, e2, NoValue);
			check(e2);
			var etmp = mk(TLocal(tmp), t_int, p);
			var incr = mk(TUnop(Increment, Postfix, etmp), t_int, p);
			var init = mk(TVar(i, Some(incr)), t_void, pi);
			var block = switch (e2.eexpr) {
			case TBlock(el): mk(TBlock(::(init, el)), t_void, e2.epos);
			case _: mk(TBlock(::(init, ::(e2, []))), t_void, p);
			};
			var i2 = switch (follow(i2.etype)) {
			case TAbstract({ a_path = ([], Int) }, []): i2;
			case _: {
				(i2) with eexpr = TCast(i2, None);
				etype = t_int
			};
			};
			switch (max) {
			case None: lblock(::(mk(TVar(tmp, Some(i1)), t_void, p), ::(mk(TWhile(mk(TBinop(OpLt, etmp, i2), ctx.t.tbool, p), block,
									 NormalWhile), t_void, p), [])));
			case Some(max): lblock(::(mk(TVar(tmp, Some(i1)), t_void, p), ::(mk(TVar(max, Some(i2)), t_void, p),
										  ::(mk(TWhile(mk(TBinop(OpLt, etmp, mk(TLocal(max), t_int, p)), ctx.t.tbool, p), block, NormalWhile), t_void, p), []))));
			};
		case (TArrayDecl(el), TInst({ cl_path = ([], Array) }, ::(pt, []))) if (False):
			try {
				var num_expr = ref(0);
				function loop(e) return {
					switch (fst(e)) {
					case EContinue | EBreak: raise(Exit);
					case _: incr(num_expr);
						Ast.map_expr(loop, e);
					};
				};
				ignore(loop(e2));
				var v = add_local(ctx, i, pt);
				var e2 = type_expr(ctx, e2, NoValue);
				var cost = * (List.length(el), num_expr.val);
				var max_cost = try {
					int_of_string(Common.defined_value(ctx.com, Define.LoopUnrollMaxCost));
				} catch (e: Not_found) {
					250;
				};
				if ( > (cost, max_cost)) {
					raise(Exit);
				} else {
					[];
				};
				var eloc = mk(TLocal(v), v.v_type, p);
				var el = List.map(function e: var e_assign = mk(TBinop(OpAssign, eloc, e), e.etype, e.epos);
								  concat(e_assign, e2), el);
				var ev = mk(TVar(v, None), ctx.t.tvoid, p);
				Some(mk(TBlock(::(ev, el)), ctx.t.tvoid, p));
			} catch (e: Exit) {
				gen_int_iter(pt, get_next_array_element, get_array_length);
			};
		case (_, TInst({ cl_path = ([], Array) }, ::(pt, []))) | (_, TInst({ cl_path = (::(flash, []), Vector) }, ::(pt, []))):
			gen_int_iter(pt, get_next_array_element, get_array_length);
		case (_, TInst({ cl_array_access = Some(pt) } = c, pl)) if (&&(try {
					switch (follow(PMap.find("length", c.cl_fields).cf_type)) {
						case TAbstract({ a_path = ([], Int) }, []): True;
						case _: False;
						};
					} catch (e: Not_found) {
						False;
					}, !(PMap.mem("iterator", c.cl_fields)))): gen_int_iter(apply_params(c.cl_params, pl, pt), get_next_array_element,
					get_array_length);
		case (_, TAbstract({ a_impl = Some(c) } = a, tl)):
			try {
				var cf_length = PMap.find("get_length", c.cl_statics);
				function get_length(e, p) return {
					make_static_call(ctx, c, cf_length, apply_params(a.a_params, tl), ::(e, []), ctx.com.basic.tint, p);
				};
				switch (follow(cf_length.cf_type)) {
				case TFun(_, tr): switch (follow(tr)) {
					case TAbstract({ a_path = ([], Int) }, _): [];
					case _: raise(Not_found);
					};
				case _: raise(Not_found);
				};
				try {
					var todo = mk(TConst(TNull), ctx.t.tint, p);
					var Tuple(cf, _, r, _, _) = find_array_access_raise_ref.val(ctx, a, tl, todo, None, p);
					function get_next(e_base, e_index, t, p) return {
						make_static_call(ctx, c, cf, apply_params(a.a_params, tl), ::(e_base, ::(e_index, [])), r, p);
					};
					gen_int_iter(r, get_next, get_length);
				} catch (e: Not_found) {
					if (!(Meta.has(Meta.ArrayAccess, a.a_meta))) {
						raise(Not_found);
					} else {
						[];
					};
					if (!(Meta.has(Meta.CoreType, a.a_meta))) {
						raise(Not_found);
					} else {
						[];
					};
					var t = switch (tl) {
					case ::(t, []): t;
					case _: raise(Not_found);
					};
					gen_int_iter(t, get_next_array_element, get_length);
				};
			} catch (e: Not_found) {
				None;
			};
		case (_, TInst({ cl_kind = KGenericInstance({ cl_path = (::(haxe, ::(ds, [])), GenericStack) }, ::(t, [])) } = c, [])): var
			tcell = try {
				PMap.find("head", c.cl_fields).cf_type;
			} catch (e: Not_found) {
				assert False;
			};
			var i = add_local(ctx, i, t);
			var cell = gen_local(ctx, tcell);
			var cexpr = mk(TLocal(cell), tcell, p);
			var e2 = type_expr(ctx, e2, NoValue);
			var evar = mk(TVar(i, Some(mk(mk_field(cexpr, "elt"), t, p))), t_void, pi);
			var enext = mk(TBinop(OpAssign, cexpr, mk(mk_field(cexpr, "next"), tcell, p)), tcell, p);
			var block = switch (e2.eexpr) {
			case TBlock(el): mk(TBlock(::(evar, ::(enext, el))), t_void, e2.epos);
			case _: mk(TBlock(::(evar, ::(enext, ::(e2, [])))), t_void, p);
			};
			lblock(::(mk(TVar(cell, Some(mk(mk_field(e1, "head"), tcell, p))), t_void, p), ::(mk(TWhile(mk(TBinop(OpNotEq, cexpr,
					  mk(TConst(TNull), tcell, p)), ctx.t.tbool, p), block, NormalWhile), t_void, p), [])));
		case _: None;
		};
	};

	public static function optimize_for_loop_iterator(ctx, v, e1, e2, p) return {
		var Tuple(c, tl) = switch (follow(e1.etype)) {
		case TInst(c, pl): (new Tuple(c, pl));
		case _: raise(Exit);
		};
		var Tuple(_, _, fhasnext) = try {
			raw_class_field(function cf: apply_params(c.cl_params, tl, cf.cf_type), c, tl, "hasNext");
		} catch (e: Not_found) {
			raise(Exit);
		};
		if (<>(fhasnext.cf_kind, Method(MethInline))) {
			raise(Exit);
		} else {
			[];
		};
		var tmp = gen_local(ctx, e1.etype);
		var eit = mk(TLocal(tmp), e1.etype, p);
		var ehasnext = make_call(ctx, mk(TField(eit, FInstance(c, tl, fhasnext)), TFun([], ctx.t.tbool), p), [], ctx.t.tbool, p);
		var enext = mk(TVar(v, Some(make_call(ctx, mk(TField(eit, quick_field_dynamic(eit.etype, "next")), TFun([], v.v_type), p), [], v.v_type, p))), ctx.t.tvoid, p);
		var eblock = switch (e2.eexpr) {
		case TBlock(el): {
			(e2) with eexpr = TBlock(::(enext, el))
		};
		case _: mk(TBlock(::(enext, ::(e2, []))), ctx.t.tvoid, p);
		};
		mk(TBlock(::(mk(TVar(tmp, Some(e1)), ctx.t.tvoid, p), ::(mk(TWhile(ehasnext, eblock, NormalWhile), ctx.t.tvoid, p), []))), ctx.t.tvoid, p);
	};

	public static function standard_precedence(op) return {
		var left = True;
		var right = False;
		switch (op) {
		case OpMult | OpDiv | OpMod: (new Tuple(5, left));
		case OpAdd | OpSub: (new Tuple(6, left));
		case OpShl | OpShr | OpUShr: (new Tuple(7, left));
		case OpLt | OpLte | OpGt | OpGte: (new Tuple(8, left));
		case OpEq | OpNotEq: (new Tuple(9, left));
		case OpAnd: (new Tuple(10, left));
		case OpXor: (new Tuple(11, left));
		case OpOr: (new Tuple(12, left));
		case OpInterval: (new Tuple(13, right));
		case OpBoolAnd: (new Tuple(14, left));
		case OpBoolOr: (new Tuple(15, left));
		case OpArrow: (new Tuple(16, left));
		case OpAssignOp(OpAssign): (new Tuple(17, right));
		case OpAssign | OpAssignOp(_): (new Tuple(18, right));
		};
	};

	public static function need_parent(e) return {
		switch (e.eexpr) {
		case TConst(_) | TLocal(_) | TArray(_) | TField(_) | TEnumParameter(_) | TParenthesis(_) | TMeta(_) | TCall(_) | TNew(_) | TTypeExpr(_) | TObjectDecl(_) | TArrayDecl(_)
				: False;
		case TCast(e, None): need_parent(e);
		case TCast(_) | TThrow(_) | TReturn(_) | TTry(_) | TSwitch(_) | TFor(_) | TIf(_) | TWhile(_) | TBinop(_) | TContinue | TBreak | TBlock(_) | TVar(_) | TFunction(_) | TUnop(_)
				: True;
		};
	};

	public static function sanitize_expr(com, e) return {
		function parent(e) return {
			switch (e.eexpr) {
			case TParenthesis(_): e;
			case _: mk(TParenthesis(e), e.etype, e.epos);
			};
		};
		function block(e) return {
			switch (e.eexpr) {
			case TBlock(_): e;
			case _: mk(TBlock(::(e, [])), e.etype, e.epos);
			};
		};
		function complex(e) return {
			switch (e.eexpr) {
			case TVar(_) | TFor(_) | TCall({ eexpr = TLocal({ v_name = __js__ }) }, _): block(e);
			case _: e;
			};
		};
		function has_if(e) return {
			switch (e.eexpr) {
			case TIf(_, _, None): True;
			case TWhile(_, e, NormalWhile): has_if(e);
			case TFor(_, _, e): has_if(e);
			case _: False;
			};
		};
		switch (e.eexpr) {
		case TConst(TNull): if ( && (com.config.pf_static, !(is_nullable(e.etype)))) {
				function loop(t) return {
					switch (follow(t)) {
					case TMono(_): [];
					case TFun(_): [];
					case TAbstract(a, tl) if (!(Meta.has(Meta.CoreType, a.a_meta))): loop(apply_params(a.a_params, tl, a.a_this));
					case _: com.error( ^ ("On static platforms, null can't be used as basic type ", s_type(print_context([]), e.etype)),
						e.epos);
					};
				};
				loop(e.etype);
			} else {
				[];
			};
			e;
		case TBinop(op, e1, e2): 	function swap(op1, op2) return {
				var Tuple(p1, left1) = standard_precedence(op1);
				var Tuple(p2, _) = standard_precedence(op2);
				&& (left1, <= (p1, p2));
			};
			function loop(ee, left) return {
				switch (ee.eexpr) {
				case TBinop(op2, _, _): if (left) {
						!(swap(op2, op));
					} else {
						swap(op, op2);
					};
				case TIf(_): if (left) {
						!(swap(OpAssignOp(OpAssign), op));
					} else {
						swap(op, OpAssignOp(OpAssign));
					};
				case TCast(e, None): loop(e, left);
				case _: False;
				};
			};
			var e1 = if (loop(e1, True)) {
				parent(e1);
			} else {
				e1;
			};
			var e2 = if (loop(e2, False)) {
				parent(e2);
			} else {
				e2;
			};
			{
				(e) with eexpr = TBinop(op, e1, e2)
			};
		case TUnop(op, mode, e1): 	function loop(ee) return {
				switch (ee.eexpr) {
				case TBinop(_) | TIf(_) | TUnop(_): parent(e1);
				case TCast(e, None): loop(e);
				case _: e1;
				};
			};
			{
				(e) with eexpr = TUnop(op, mode, loop(e1))
			};
		case TIf(e1, e2, eelse): var e1 = parent(e1);
			var e2 = if ( || ( && (<>(eelse, None), has_if(e2)), switch (e2.eexpr) {
			case TIf(_): True;
				case _: False;
				})) {
				block(e2);
			} else {
				complex(e2);
			};
			var eelse = switch (eelse) {
			case None: None;
			case Some(e): Some(complex(e));
			};
			{
				(e) with eexpr = TIf(e1, e2, eelse)
			};
		case TWhile(e1, e2, flag): var e1 = parent(e1);
			var e2 = complex(e2);
			{
				(e) with eexpr = TWhile(e1, e2, flag)
			};
		case TFor(v, e1, e2): var e2 = complex(e2);
			{
				(e) with eexpr = TFor(v, e1, e2)
			};
		case TFunction(f): var f = switch (f.tf_expr.eexpr) {
			case TBlock(_): f;
			case _: {
				(f) with tf_expr = block(f.tf_expr)
			};
			};
			{
				(e) with eexpr = TFunction(f)
			};
		case TCall(e2, args): if (need_parent(e2)) {
				{
					(e) with eexpr = TCall(parent(e2), args)
				};
			} else {
				e;
			};
		case TEnumParameter(e2, ef, i): if (need_parent(e2)) {
				{
					(e) with eexpr = TEnumParameter(parent(e2), ef, i)
				};
			} else {
				e;
			};
		case TField(e2, f): if (need_parent(e2)) {
				{
					(e) with eexpr = TField(parent(e2), f)
				};
			} else {
				e;
			};
		case TArray(e1, e2): if (need_parent(e1)) {
				{
					(e) with eexpr = TArray(parent(e1), e2)
				};
			} else {
				e;
			};
		case TTry(e1, catches): var e1 = block(e1);
			var catches = List.map(function (v, e): (new Tuple(v, block(e))), catches);
			{
				(e) with eexpr = TTry(e1, catches)
			};
		case TSwitch(e1, cases, def): var e1 = parent(e1);
			var cases = List.map(function (el, e): (new Tuple(el, complex(e))), cases);
			var def = switch (def) {
			case None: None;
			case Some(e): Some(complex(e));
			};
			{
				(e) with eexpr = TSwitch(e1, cases, def)
			};
		case _: e;
		};
	};

	public static function reduce_expr(com, e) return {
		switch (e.eexpr) {
		case TSwitch(_, cases, _): List.iter(function (cl, _): List.iter(function e: switch (e.eexpr) {
		case TCall({ eexpr = TField(_, FEnum(_)) }, _): error("Not-constant enum in switch cannot be matched", e.epos);
			case _: [];
			}, cl), cases);
			e;
		case TBlock(l): switch (List.rev(l)) {
			case []: e;
			case ::(ec, l): switch (List.filter(function e:
				switch (e.eexpr) {
				case TConst(_) | TBlock([]) | TObjectDecl([]): False;
					case _: True;
					}, l)) {
				case []: ec;
				case l: {
					(e) with eexpr = TBlock(List.rev(::(ec, l)))
				};
				};
			};
		case TParenthesis(ec): {
			(ec) with epos = e.epos
		};
		case TTry(e, []): e;
		case _: e;
		};
	};

	public static function sanitize(com, e) return {
		sanitize_expr(com, reduce_expr(com, Type.map_expr(sanitize(com), e)));
	};

	public static function optimize_binop(e, op, e1, e2) return {
		function is_float(t) return {
			switch (follow(t)) {
			case TAbstract({ a_path = ([], Float) }, _): True;
			case _: False;
			};
		};
		function is_numeric(t) return {
			switch (follow(t)) {
			case TAbstract({ a_path = ([], Float | Int) }, _): True;
			case _: False;
			};
		};
		function check_float(op, f1, f2) return {
			var f = op(f1, f2);
			var fstr = float_repres(f);
			if (switch (classify_float(f)) {
			case FP_nan | FP_infinite: False;
			case _: = (float_of_string(fstr), f);
				}) {
				{
					(e) with eexpr = TConst(TFloat(fstr))
				};
			} else {
				e;
			};
		};
		switch ((new Tuple(e1.eexpr, e2.eexpr))) {
		case (TConst(TInt(undefined)), _) if (&&(=(op, OpAdd), is_numeric(e2.etype))): e2;
		case (TConst(TInt(undefined)), _) if (=(op, OpMult)): e2;
		case (TConst(TFloat(v)), _) if (&&(=(op, OpAdd), &&(=(float_of_string(v), 0.), is_float(e2.etype)))): e2;
		case (TConst(TFloat(v)), _) if (&&(=(op, OpMult), &&(=(float_of_string(v), 1.), is_float(e2.etype)))): e2;
		case (_, TConst(TInt(undefined))) if (switch (op) {
				case OpAdd: is_numeric(e1.etype);
					case OpSub | OpShr | OpShl: True;
					case _: False;
					}): e1;
		case (_, TConst(TInt(undefined))) if (=(op, OpMult)): e1;
		case (_, TConst(TFloat(v))) if (switch (op) {
				case OpAdd | OpSub: && ( = (float_of_string(v), 0.), is_float(e1.etype));
					case _: False;
					}): e1;
		case (_, TConst(TFloat(v))) if (&&(=(op, OpMult), &&(=(float_of_string(v), 1.), is_float(e1.etype)))): e1;
		case (TConst(TNull), TConst(TNull)): switch (op) {
			case OpEq: {
				(e) with eexpr = TConst(TBool(True))
			};
			case OpNotEq: {
				(e) with eexpr = TConst(TBool(False))
			};
			case _: e;
			};
		case (TFunction(_), TConst(TNull)): switch (op) {
			case OpEq: {
				(e) with eexpr = TConst(TBool(False))
			};
			case OpNotEq: {
				(e) with eexpr = TConst(TBool(True))
			};
			case _: e;
			};
		case (TConst(TNull), TFunction(_)): switch (op) {
			case OpEq: {
				(e) with eexpr = TConst(TBool(False))
			};
			case OpNotEq: {
				(e) with eexpr = TConst(TBool(True))
			};
			case _: e;
			};
		case (TConst(TInt(a)), TConst(TInt(b))): 	function opt(f) return {
				try {
					{ (e) with eexpr = TConst(TInt(f(a, b))) };
				} catch (e: Exit) {
					e;
				};
			};
			function check_overflow(f) return {
				opt(function a: function b: var v = f(Int64.of_int32(a), Int64.of_int32(b));
				var iv = Int64.to_int32(v);
				if (<>(Int64.compare(Int64.of_int32(iv), v), 0)) {
				raise(Exit);
				} else {
					[];
				};
				iv);
			};
			function ebool(t) return {
				{ (e) with eexpr = TConst(TBool(t(Int32.compare(a, b), 0))) };
			};
			switch (op) {
			case OpAdd: check_overflow(Int64.add);
			case OpSub: check_overflow(Int64.sub);
			case OpMult: check_overflow(Int64.mul);
			case OpDiv: check_float( / ., Int32.to_float(a), Int32.to_float(b));
			case OpAnd: opt(Int32.logand);
			case OpOr: opt(Int32.logor);
			case OpXor: opt(Int32.logxor);
			case OpShl: opt(function a: function b: Int32.shift_left(a, Int32.to_int(b)));
			case OpShr: opt(function a: function b: Int32.shift_right(a, Int32.to_int(b)));
			case OpUShr: opt(function a: function b: Int32.shift_right_logical(a, Int32.to_int(b)));
			case OpEq: ebool( = );
			case OpNotEq: ebool(<>);
			case OpGt: ebool( > );
			case OpGte: ebool( >= );
			case OpLt: ebool( < );
			case OpLte: ebool( <= );
			case _: e;
			};
		case (TConst(TFloat(_) | TInt(_) = ca), TConst(TFloat(_) | TInt(_) = cb)): var fa = switch (ca) {
			case TFloat(a): float_of_string(a);
			case TInt(a): Int32.to_float(a);
			case _: assert False;
			};
			var fb = switch (cb) {
			case TFloat(b): float_of_string(b);
			case TInt(b): Int32.to_float(b);
			case _: assert False;
			};
			function fop(op) return {
				check_float(op, fa, fb);
			};
			function ebool(t) return {
				{ (e) with eexpr = TConst(TBool(t(compare(fa, fb), 0))) };
			};
			switch (op) {
			case OpAdd: fop(+.);
			case OpDiv: fop( / .);
			case OpSub: fop(-.);
			case OpMult: fop( * .);
			case OpEq: ebool( = );
			case OpNotEq: ebool(<>);
			case OpGt: ebool( > );
			case OpGte: ebool( >= );
			case OpLt: ebool( < );
			case OpLte: ebool( <= );
			case _: e;
			};
		case (TConst(TBool(a)), TConst(TBool(b))): 	function ebool(f) return {
				{ (e) with eexpr = TConst(TBool(f(a, b))) };
			};
			switch (op) {
			case OpEq: ebool( = );
			case OpNotEq: ebool(<>);
			case OpBoolAnd: ebool( && );
			case OpBoolOr: ebool( || );
			case _: e;
			};
		case (TConst(a), TConst(b)) if (||(=(op, OpEq), =(op, OpNotEq))): 	function ebool(b) return {
				{
					(e) with eexpr = TConst(TBool(if ( = (op, OpEq)) {
					b;
				} else {
					!(b);
					}))
				};
			};
			switch ((new Tuple(a, b))) {
			case (TInt(a), TFloat(b)) | (TFloat(b), TInt(a)): ebool( = (Int32.to_float(a), float_of_string(b)));
			case _: ebool( = (a, b));
			};
		case (TConst(TBool(a)), _): switch (op) {
			case OpBoolAnd: if (a) {
					e2;
				} else {
					{
						(e) with eexpr = TConst(TBool(False))
					};
				};
			case OpBoolOr: if (a) {
					{
						(e) with eexpr = TConst(TBool(True))
					};
				} else {
					e2;
				};
			case _: e;
			};
		case (_, TConst(TBool(a))): switch (op) {
			case OpBoolAnd if (a): e1;
			case OpBoolOr if (!(a)): e1;
			case _: e;
			};
		case (TField(_, FEnum(e1, f1)), TField(_, FEnum(e2, f2))) if (==(e1, e2)):
			switch (op) {
			case OpEq: {
				(e) with eexpr = TConst(TBool( == (f1, f2)))
			};
			case OpNotEq: {
				(e) with eexpr = TConst(TBool( != (f1, f2)))
			};
			case _: e;
			};
		case (_, TCall({ eexpr = TField(_, FEnum(_)) }, _)) | (TCall({ eexpr = TField(_, FEnum(_)) }, _), _):
			switch (op) {
			case OpAssign: e;
			case _: error("You cannot directly compare enums with arguments. Use either 'switch' or 'Type.enumEq'", e.epos);
			};
		case _: e;
		};
	};

	public static function optimize_unop(e, op, flag, esub) return {
		switch ((new Tuple(op, esub.eexpr))) {
		case (Not, TConst(TBool(f)) | TParenthesis({ eexpr = TConst(TBool(f)) })): {
			(e) with eexpr = TConst(TBool(!(f)))
		};
		case (Neg, TConst(TInt(i))): {
			(e) with eexpr = TConst(TInt(Int32.neg(i)))
		};
		case (NegBits, TConst(TInt(i))): {
			(e) with eexpr = TConst(TInt(Int32.lognot(i)))
		};
		case (Neg, TConst(TFloat(f))): var v = -.(0., float_of_string(f));
			var vstr = float_repres(v);
			if ( = (float_of_string(vstr), v)) {
				{
					(e) with eexpr = TConst(TFloat(vstr))
				};
			} else {
				e;
			};
		case _: e;
		};
	};

	public static function reduce_loop(ctx, e) return {
		var e = Type.map_expr(reduce_loop(ctx), e);
		sanitize_expr(ctx.com, switch (e.eexpr) {
	case TIf({ eexpr = TConst(TBool(t)) }, e1, e2):
			if (t) {
				e1;
			} else {
				switch (e2) {
				case None: {
					(e) with eexpr = TBlock([])
				};
				case Some(e): e;
				};
			};
		case TWhile({ eexpr = TConst(TBool(False)) }, sub, flag):
			switch (flag) {
			case NormalWhile: {
				(e) with eexpr = TBlock([])
			};
			case DoWhile: e;
			};
		case TBinop(op, e1, e2): optimize_binop(e, op, e1, e2);
		case TUnop(op, flag, esub): optimize_unop(e, op, flag, esub);
		case TCall({ eexpr = TField({ eexpr = TTypeExpr(TClassDecl(c)) }, field) }, params):
			switch (api_inline(ctx, c, field_name(field), params, e.epos)) {
			case None: reduce_expr(ctx, e);
			case Some(e): reduce_loop(ctx, e);
			};
		case TCall({ eexpr = TFunction(func) } = ef, el): var cf = mk_field("", ef.etype, e.epos);
			var ethis = mk(TConst(TThis), t_dynamic, e.epos);
			var rt = switch (follow(ef.etype)) {
			case TFun(_, rt): rt;
			case _: assert False;
			};
			var inl = try {
				type_inline(ctx, cf, func, ethis, el, rt, None, e.epos, self_calling_closure = True, False);
			} catch (e: Error(Custom(_))(_)) {
				None;
			};
			switch (inl) {
			case None: reduce_expr(ctx, e);
			case Some(e): reduce_loop(ctx, e);
			};
		case TCall({ eexpr = TField(o, FClosure(c, cf)) } = f, el): var fmode = switch (c) {
			case None: FAnon(cf);
			case Some(c, tl): FInstance(c, tl, cf);
			};
			{
				(e) with eexpr = TCall({ (f) with eexpr = TField(o, fmode) }, el)
			};
		case TSwitch(e1, ::((::({ eexpr = TConst(TBool(True)) }, []), { eexpr = TConst(TBool(True)) }), []), Some({ eexpr = TConst(TBool(False)) }))
				: e1;
		case _: reduce_expr(ctx, e);
		});
	};

	public static function reduce_expression(ctx, e) return {
		if (ctx.com.foptimize) {
			reduce_loop(ctx, e);
		} else {
			e;
		};
	};

	public static function make_constant_expression(ctx, ? : (concat_strings = False), e) return {
		var e = reduce_loop(ctx, e);
		switch (e.eexpr) {
		case TConst(_): Some(e);
		case TBinop(OpAdd | OpSub | OpMult | OpDiv | OpMod = op, e1, e2): switch ((new Tuple(make_constant_expression(ctx, e1),
			make_constant_expression(ctx, e2)))) {
			case (Some({ eexpr = TConst(TString(s1)) }), Some({ eexpr = TConst(TString(s2)) })) if (concat_strings): Some(mk(TConst(
					TString( ^ (s1, s2))), ctx.com.basic.tstring, punion(e1.epos, e2.epos)));
			case (Some(e1), Some(e2)): Some(mk(TBinop(op, e1, e2), e.etype, e.epos));
			case _: None;
			};
		case TCast(e1, None): switch (make_constant_expression(ctx, e1)) {
			case None: None;
			case Some(e1): Some({ (e) with eexpr = TCast(e1, None) });
			};
		case TParenthesis(e1): switch (make_constant_expression(ctx, concat_strings = , e1)) {
			case None: None;
			case Some(e1): Some({ (e) with eexpr = TParenthesis(e1) });
			};
		case TMeta(m, e1): switch (make_constant_expression(ctx, concat_strings = , e1)) {
			case None: None;
			case Some(e1): Some({ (e) with eexpr = TMeta(m, e1) });
			};
		case TTypeExpr(_): Some(e);
		case TCall({ etype = TFun(_, ret); eexpr = TField(_, FStatic(c, cf)) }, el):
			try {
				var func = switch (cf.cf_expr) {
				case Some({ eexpr = TFunction(func) }): func;
				case _: raise(Not_found);
				};
				var ethis = mk(TConst(TThis), t_dynamic, e.epos);
				var inl = try {
					type_inline(ctx, cf, func, ethis, el, ret, None, e.epos, False);
				} catch (e: Error(Custom(_))(_)) {
					None;
				};
				switch (inl) {
				case None: None;
				case Some(e): make_constant_expression(ctx, e);
				};
			} catch (e: Not_found) {
				None;
			};
		case _: None;
		};
	};

	public static function inline_constructors(ctx, e) return {
		var vars = ref(PMap.empty);
		function is_valid_ident(s) return {
			try {
				if ( = (String.length(s), 0)) {
					raise(Exit);
				} else {
					[];
				};
				switch (String.unsafe_get(s, 0)) {
				case 'a' .. 'z' | 'A' .. 'Z' | '_': [];
				case _: raise(Exit);
				};
				for (i in /*to*/1... - (String.length(s), 1)) {
					switch (String.unsafe_get(s, i)) {
					case 'a' .. 'z' | 'A' .. 'Z' | '_': [];
					case '0' .. '9' if (>(i, 0)): [];
					case _: raise(Exit);
					};
				};
				True;
			} catch (e: Exit) {
				False;
			};
		};
		function get_inline_ctor_info(e) return {
			switch (e.eexpr) {
			case TNew({ cl_constructor = Some({ cf_kind = Method(MethInline); cf_expr = Some({ eexpr = TFunction(f) }) } = cst) } = c, tl, pl)
				: IKCtor(f, cst, c, tl, pl, []);
			case TObjectDecl([]) | TArrayDecl([]): IKNone;
			case TArrayDecl(el): switch (follow(e.etype)) {
				case TInst({ cl_path = ([], Array) }, ::(t, [])): IKArray(el, t);
				case _: IKNone;
				};
			case TObjectDecl(fl): if (List.exists(function (s, _): !(is_valid_ident(s)), fl)) {
					IKNone;
				} else {
					IKStructure(fl);
				};
			case TCast(e, None) | TParenthesis(e): get_inline_ctor_info(e);
			case TBlock(el): switch (List.rev(el)) {
				case ::(e, el): switch (get_inline_ctor_info(e)) {
					case IKCtor(f, cst, c, tl, pl, e_init): IKCtor(f, cst, c, tl, pl, @(List.rev(el), e_init));
					case _: IKNone;
					};
				case []: IKNone;
				};
			case _: IKNone;
			};
		};
		function check_field(v, s, e, t) return {
			var Tuple(a, b, fields, c, d) = PMap.find(~ -(v.v_id), vars.val);
			if (!(List.exists(function (s2, _, _): = (s, s2), fields))) {
				vars.val = PMap.add(~ -(v.v_id), (new Tuple(a, b, ::((new Tuple(s, e, t)), fields), c, d)), vars.val);
			} else {
				[];
			};
		};
		function cancel(v) return {
			v.v_id = ~ -(v.v_id);
			switch (PMap.find(v.v_id, vars.val)) {
			case (_, _, _, True, p): display_error(ctx, "Extern constructor could not be inlined", p);
				error("Variable is used here", e.epos);
			case _: [];
			};
			vars.val = PMap.remove(v.v_id, vars.val);
		};
		function skip_to_var(e) return {
			switch (e.eexpr) {
			case TLocal(v) if (<(v.v_id, 0)): Some(v);
			case _: None;
			};
		};
		function find_locals(e) return {
			switch (e.eexpr) {
			case TVar(v, eo): Type.iter(find_locals, e);
				switch (eo) {
				case Some(n): switch (get_inline_ctor_info(n)) {
					case IKCtor(f, cst, c, tl, pl, el_init) if (type_iseq(v.v_type, n.etype)):
						switch (try {
							type_inline(ctx, cst, f, mk(TLocal(v), TInst(c, tl), n.epos), pl, ctx.t.tvoid, None, n.epos, True);
							} catch (e: Error(Custom(_))(_)) {
								None;
							}) {
						case None: [];
						case Some(ecst): var assigns = ref([]);
							List.iter(function cf:
							switch ((new Tuple(cf.cf_kind, cf.cf_expr))) {
						case (Var(_), Some(e)): assigns.val = ::((new Tuple(cf.cf_name, e, cf.cf_type)), assigns.val);
							case _: [];
							}, c.cl_ordered_fields);
							function get_assigns(e) return {
								switch (e.eexpr) {
								case TBlock(el): List.iter(get_assigns, el);
								case TBinop(OpAssign, { eexpr = TField({ eexpr = TLocal(vv) }, FInstance(_, _, cf)); etype = t }, e) if (==(v, vv)):
									assigns.val = ::((new Tuple(cf.cf_name, e, t)), assigns.val);
								case _: raise(Exit);
								};
							};
							try {
								get_assigns(ecst);
								vars.val = PMap.add(v.v_id, (new Tuple(v, el_init, List.rev(assigns.val),
																	   || (c.cl_extern, Meta.has(Meta.Extern, cst.cf_meta)), n.epos)), vars.val);
								v.v_id = ~ -(v.v_id);
								find_locals(ecst);
							} catch (e: Exit) {
								[];
							};
						};
					case IKArray(el, t): vars.val = PMap.add(v.v_id, (new Tuple(v, [],
														ExtList.List.mapi(function i: function e: (new Tuple(string_of_int(i), e, t)), el), False, n.epos)), vars.val);
						v.v_id = ~ -(v.v_id);
					case IKStructure(fl): vars.val = PMap.add(v.v_id, (new Tuple(v, [], List.map(function (s, e): (new Tuple(s, e, e.etype)),
														 fl), False, n.epos)), vars.val);
						v.v_id = ~ -(v.v_id);
					case _: [];
					};
				case _: [];
				};
			case TField(e1, FInstance(_, _, { cf_kind = Var(_); cf_name = s }) | FAnon({ cf_kind = Var(_); cf_name = s })):
				switch (skip_to_var(e1)) {
				case None: find_locals(e1);
				case Some(_): [];
				};
			case TArray(e1, { eexpr = TConst(TInt(i)) }):
				switch (skip_to_var(e1)) {
				case None: find_locals(e1);
				case Some(v): var Tuple(_, _, fields, _, _) = PMap.find(~ -(v.v_id), vars.val);
					var i = Int32.to_int(i);
					if ( || (<(i, 0), >= (i, List.length(fields)))) {
						cancel(v);
					} else {
						[];
					};
				};
			case TBinop(OpAssign | OpAssignOp(_), e1, e2): switch (e1.eexpr) {
				case TArray({ eexpr = TLocal(v) }, { eexpr = TConst(TInt(i)) }) if (<(v.v_id, 0)): check_field(v, Int32.to_string(i), e2,
							e2.etype);
				case TField({ eexpr = TLocal(v) }, FInstance(_, _, { cf_kind = Var(_); cf_name = s }) | FAnon({ cf_kind = Var(_); cf_name = s })) if (<(v.v_id, 0))
							: check_field(v, s, e2, e2.etype);
				case _: find_locals(e1);
				};
				find_locals(e2);
			case TLocal(v) if (<(v.v_id, 0)): cancel(v);
			case _: Type.iter(find_locals, e);
			};
		};
		find_locals(e);
		var vars = vars.val;
		if (PMap.is_empty(vars)) {
			e;
		} else {
			var vfields = PMap.map(function (v, el_init, assigns, _, _): (new Tuple(List.fold_left(function (acc, map): function (name, e, t): var vf = alloc_var( ^ (v.v_name, ^ ("_", name)), t);
			(new Tuple(::((new Tuple(vf, e)), acc), PMap.add(name, vf, map))), (new Tuple([], PMap.empty)), assigns), el_init)), vars);
			var el_b = ref([]);
			function append(e) return {
				el_b.val = ::(e, el_b.val);
			};
			function inline_field(c, cf, v) return {
				var Tuple(Tuple(_, vars), el_init) = PMap.find(~ -(v.v_id), vfields);
				try {
					var v = PMap.find(cf.cf_name, vars);
					mk(TLocal(v), v.v_type, e.epos);
				} catch (e: Not_found) {
					if ( && ( = (c.cl_path, (new Tuple([], "Array"))), = (cf.cf_name, "length"))) {
						var l = PMap.fold(function _: function i: +(i, 1), vars, 0);
						mk(TConst(TInt(Int32.of_int(l))), ctx.t.tint, e.epos);
					} else {
						mk(TConst(TNull), e.etype, e.epos);
					};
				};
			};
			function inline_anon_field(cf, v) return {
				var Tuple(Tuple(_, vars), _) = PMap.find(~ -(v.v_id), vfields);
				try {
					var v = PMap.find(cf.cf_name, vars);
					mk(TLocal(v), v.v_type, e.epos);
				} catch (e: Not_found) {
					mk(TConst(TNull), e.etype, e.epos);
				};
			};
			function inline_array_access(i, v) return {
				var Tuple(Tuple(_, vars), _) = PMap.find(~ -(v.v_id), vfields);
				try {
					var v = PMap.find(Int32.to_string(i), vars);
					mk(TLocal(v), v.v_type, e.epos);
				} catch (e: Not_found) {
					mk(TConst(TNull), e.etype, e.epos);
				};
			};
			function subst(e) return {
				switch (e.eexpr) {
				case TBlock(el): var old = el_b.val;
					el_b.val = [];
					List.iter(function e: append(subst(e)), el);
					var n = el_b.val;
					el_b.val = old;
					{
						(e) with eexpr = TBlock(List.rev(n))
					};
				case TVar(v, Some(e)) if (<(v.v_id, 0)): var Tuple(Tuple(vars, _), el_init) = PMap.find(~ -(v.v_id), vfields);
					List.iter(function e: append(subst(e)), el_init);
					var Tuple(Tuple(v_first, e_first), vars) = switch (vars) {
					case ::(v, vl): (new Tuple(v, vl));
					case []: assert False;
					};
					List.iter(function (v, e): append(mk(TVar(v, Some(subst(e))), ctx.t.tvoid, e.epos)), List.rev(vars));
					mk(TVar(v_first, Some(subst(e_first))), ctx.t.tvoid, e.epos);
				case TField(e1, FInstance(c, _, cf)): switch (skip_to_var(e1)) {
					case None: Type.map_expr(subst, e);
					case Some(v): inline_field(c, cf, v);
					};
				case TArray(e1, { eexpr = TConst(TInt(i)) }):
					switch (skip_to_var(e1)) {
					case None: Type.map_expr(subst, e);
					case Some(v): inline_array_access(i, v);
					};
				case TField(e1, FAnon(cf)): switch (skip_to_var(e1)) {
					case None: Type.map_expr(subst, e);
					case Some(v): inline_anon_field(cf, v);
					};
				case _: Type.map_expr(subst, e);
				};
			};
			var e = try {
				subst(e);
			} catch (e: Not_found) {
				assert False;
			};
			PMap.iter(function _: function (v, _, _, _, _): v.v_id = ~ -(v.v_id), vars);
			e;
		};
	};

	public static function optimize_completion_expr(e) return {
		var iid = ref(0);
		var typing_side_effect = ref(False);
		var locals = { () with r = PMap.empty } : compl_locals;
		function save([]) return {
			var old = locals.r;
			function []: locals.r = old;
		};
		function get_local(n) return {
			PMap.find(n, locals.r);
		};
		function maybe_typed(e) return {
			switch (fst(e)) {
			case EConst(Ident(null)): False;
			case _: True;
			};
		};
		function decl(n, t, e) return {
			typing_side_effect.val = True;
			locals.r = PMap.add(n, (new Tuple(t, switch (e) {
		case Some(e) if (maybe_typed(e)): incr(iid);
				Some(iid.val, e, { () with r = locals.r });
			case _: None;
			})), locals.r);
		};
		function loop(e) return {
			var p = snd(e);
			switch (fst(e)) {
			case EConst(Ident(n)): try {
					switch (get_local(n)) {
					case (Some(_), _): [];
					case _: typing_side_effect.val = True;
					};
				} catch (e: Not_found) {
					[];
				};
				e;
			case EBinop(OpAssign, (EConst(Ident(n)), _), esub): try {
					switch (get_local(n)) {
					case (None, None) if (maybe_typed(esub)): decl(n, None, Some(esub));
					case _: [];
					};
				} catch (e: Not_found) {
					[];
				};
				map(e);
			case EVars(vl): var vl = List.map(function (v, t, e): var e = switch (e) {
			case None: None;
			case Some(e): Some(loop(e));
				};
				decl(v, t, e);
				(new Tuple(v, t, e)), vl);
				(new Tuple(EVars(vl), p));
			case EBlock(el): var old = save([]);
				var told = ref(typing_side_effect.val);
				var el = List.fold_left(function acc: function e: typing_side_effect.val = False;
										var e = loop(e);
				if (typing_side_effect.val) {
				told.val = True;
				::(e, acc);
				} else {
					acc;
				}, [], el);
				old([]);
				typing_side_effect.val = told.val;
				(new Tuple(EBlock(List.rev(el)), p));
			case EFunction(v, f): switch (v) {
				case None: [];
				case Some(name): decl(name, None, Some(e));
				};
				var old = save([]);
				List.iter(function (n, _, t, e): decl(n, t, e), f.f_args);
				var e = map(e);
				old([]);
				e;
			case EFor((EIn((EConst(Ident(n)), _) = id, it), p), efor): var it = loop(it);
				var old = save([]);
				var etmp = (new Tuple(EConst(Ident("$tmp")), p));
				decl(n, None, Some(EBlock(::((new Tuple(EVars(::((new Tuple("$tmp", None, None)), [])), p)),
											 ::((new Tuple(EFor((new Tuple(EIn(id, it), p)), (new Tuple(EBinop(OpAssign, etmp, (new Tuple(EConst(Ident(n)), p))), p))),
													 p)), ::(etmp, [])))), p));
				var efor = loop(efor);
				old([]);
				(new Tuple(EFor((new Tuple(EIn(id, it), p)), efor), p));
			case EReturn(_): typing_side_effect.val = True;
				map(e);
			case ESwitch(e, cases, def): var e = loop(e);
				var cases = List.map(function (el, eg, eo):
				switch (eo) {
			case None: (new Tuple(el, eg, eo));
				case Some(e): var el = List.map(loop, el);
					var old = save([]);
					List.iter(function e:
					switch (fst(e)) {
				case ECall(_, pl): List.iter(function p: switch (fst(p)) {
					case EConst(Ident(i)): decl(i, None, None);
						case _: [];
						}, pl);
					case _: [];
					}, el);
					var e = loop(e);
					old([]);
					(new Tuple(el, eg, Some(e)));
				}, cases);
				var def = switch (def) {
				case None: None;
				case Some(None): Some(None);
				case Some(Some(e)): Some(Some(loop(e)));
				};
				(new Tuple(ESwitch(e, cases, def), p));
			case ETry(et, cl): var et = loop(et);
				var cl = List.map(function (n, t, e): var old = save([]);
								  decl(n, Some(t), None);
								  var e = loop(e);
								  old([]);
								  (new Tuple(n, t, e)), cl);
				(new Tuple(ETry(et, cl), p));
			case EDisplay(s, call): typing_side_effect.val = True;
				var tmp_locals = ref([]);
				var tmp_hlocals = ref(PMap.empty);
				function subst_locals(locals, e) return {
					switch (fst(e)) {
					case EConst(Ident(n)): var p = snd(e);
						try {
							switch (PMap.find(n, locals.r)) {
							case (Some(t), _): (new Tuple(ECheckType((new Tuple(EConst(Ident("null")), p)), t), p));
							case (_, Some(id, e, lc)): var name = try {
									PMap.find(id, tmp_hlocals.val);
								} catch (e: Not_found) {
									var e = subst_locals(lc, e);
									var name = ^ ("$tmp_", string_of_int(id));
									tmp_locals.val = ::((new Tuple(name, None, Some(e))), tmp_locals.val);
									tmp_hlocals.val = PMap.add(id, name, tmp_hlocals.val);
									name;
								};
								(new Tuple(EConst(Ident(name)), p));
							case (None, None): raise(Exit);
							};
						} catch (e: Not_found) {
							e;
						};
					case EFunction(_, f): Ast.map_expr(subst_locals({ () with r = PMap.foldi(function n: function i: function acc:
						if (List.exists(function (a, _, _, _): = (a, n), f.f_args)) {
						acc;
					} else {
						PMap.add(n, i, acc);
						}, locals.r, PMap.empty)
																		}), e);
					case EObjectDecl([]): raise(Exit);
					case _: Ast.map_expr(subst_locals(locals), e);
					};
				};
				try {
					var e = subst_locals(locals, s);
					var e = (new Tuple(EBlock(::((new Tuple(EVars(List.rev(tmp_locals.val)), p)), ::((new Tuple(EDisplay(e, call), p)), []))),
									   p));
					raise(Return(e));
				} catch (e: Exit) {
					map(e);
				};
			case EDisplayNew(_): raise(Return(e));
			case _: map(e);
			};
		};
		function map(e) return {
			Ast.map_expr(loop, e);
		};
		try {
			loop(e);
		} catch (e: Return(e)) {
			e;
		};
	}
}
;
