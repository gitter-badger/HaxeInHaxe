import Ast;
import Common;
import Type;
import Typecore;

enum Usage {
	Block(value: Usage -> Unit -> Unit);
	Loop(value: Usage -> Unit -> Unit);
	Function(value: Usage -> Unit -> Unit);
	Declare(value: Tvar);
	Use(value: Tvar);
	Assign(value: Tvar);
};

class Filters {
	public static function verify_ast(ctx, e) return {
		function not_null(e, e1) return {
			switch (e1.eexpr) {
			case TConst(TNull): [];
			case _: [];
			};
		};
		function loop(e) return {
			switch (e.eexpr) {
			case TField(e1, _): not_null(e, e1);
				[];
			case TArray(e1, e2): not_null(e, e1);
				loop(e1);
				loop(e2);
			case TCall(e1, el): not_null(e, e1);
				loop(e1);
				List.iter(loop, el);
			case TUnop(_, _, e1): not_null(e, e1);
				loop(e1);
			case TTypeExpr(TClassDecl({ cl_kind = KAbstractImpl(a) })) if (!(Meta.has(Meta.RuntimeValue, a.a_meta))):
				error("Cannot use abstract as value", e.epos);
			case _: Type.iter(loop, e);
			};
		};
		loop(e);
	};

	public static function blockify_ast(e) return {
		switch (e.eexpr) {
		case TIf(e1, e2, eo): {
			(e) with eexpr = TIf(blockify_ast(e1), mk_block(blockify_ast(e2)), switch (eo) {
		case None: None;
		case Some(e): Some(mk_block(blockify_ast(e)));
			})
		};
		case TFor(v, e1, e2): {
			(e) with eexpr = TFor(v, blockify_ast(e1), mk_block(blockify_ast(e2)))
		};
		case TWhile(e1, e2, flag): {
			(e) with eexpr = TWhile(blockify_ast(e1), mk_block(blockify_ast(e2)), flag)
		};
		case TFunction(tf): {
			(e) with eexpr = TFunction({ (tf) with tf_expr = mk_block(blockify_ast(tf.tf_expr)) })
		};
		case TTry(e1, cl): {
			(e) with eexpr = TTry(mk_block(blockify_ast(e1)), List.map(function (v, e): (new Tuple(v, mk_block(blockify_ast(e)))),
								  cl))
		};
		case TSwitch(e1, cases, def): var e1 = blockify_ast(e1);
			var cases = List.map(function (el, e): (new Tuple(el, mk_block(blockify_ast(e)))), cases);
			var def = switch (def) {
			case None: None;
			case Some(e): Some(mk_block(blockify_ast(e)));
			};
			{
				(e) with eexpr = TSwitch(e1, cases, def)
			};
		case _: Type.map_expr(blockify_ast, e);
		};
	};

	public static function promote_complex_rhs(com, e) return {
		function is_complex(e) return {
			switch (e.eexpr) {
			case TBlock(_) | TSwitch(_) | TIf(_) | TTry(_) | TCast(_, Some(_)): True;
			case TBinop(_, e1, e2): || (is_complex(e1), is_complex(e2));
			case TParenthesis(e) | TMeta(_, e) | TCast(e, None) | TField(e, _): is_complex(e);
			case _: False;
			};
		};
		function loop(f, e) return {
			switch (e.eexpr) {
			case TBlock(el): switch (List.rev(el)) {
				case ::(elast, el): {
					(e) with eexpr = TBlock(block(List.rev(::(loop(f, elast), el))))
				};
				case []: e;
				};
			case TSwitch(es, cases, edef): {
				(e) with eexpr = TSwitch(es, List.map(function (el, e): (new Tuple(List.map(find, el), loop(f, e))), cases),
				switch (edef) {
			case None: None;
			case Some(e): Some(loop(f, e));
				})
			};
			case TIf(eif, ethen, eelse): {
				(e) with eexpr = TIf(find(eif), loop(f, ethen), switch (eelse) {
			case None: None;
			case Some(e): Some(loop(f, e));
				})
			};
			case TTry(e1, el): {
				(e) with eexpr = TTry(loop(f, e1), List.map(function (el, e): (new Tuple(el, loop(f, e))), el))
			};
			case TParenthesis(e1) if (!(Common.defined(com, Define.As3))): {
				(e) with eexpr = TParenthesis(loop(f, e1))
			};
			case TMeta(m, e1): {
				(e) with eexpr = TMeta(m, loop(f, e1))
			};
			case TReturn(_) | TThrow(_): find(e);
			case TContinue | TBreak: e;
			case _: f(find(e));
			};
		};
		function block(el) return {
			var r = ref([]);
			List.iter(function e:
			switch (e.eexpr) {
		case TVar(v, eo): switch (eo) {
				case Some(e) if (is_complex(e)): var e = find(e);
					r.val = ::(loop(function e: mk(TBinop(OpAssign, mk(TLocal(v), v.v_type, e.epos), e), v.v_type, e.epos), e), ::(mk(TVar(v,
					None), com.basic.tvoid, e.epos), r.val));
				case Some(e): r.val = ::(mk(TVar(v, Some(find(e))), com.basic.tvoid, e.epos), r.val);
				case None: r.val = ::(mk(TVar(v, None), com.basic.tvoid, e.epos), r.val);
				};
			case TReturn(Some(e1)) if (switch (follow(e1.etype)) {
					case TAbstract({ a_path = ([], Void) }, _): True;
						case _: False;
						}): r.val = ::({ (e) with eexpr = TReturn(None) }, ::(e1, r.val));
			case _: r.val = ::(find(e), r.val);
			}, el);
			List.rev(r.val);
		};
		function find(e) return {
			switch (e.eexpr) {
			case TReturn(Some(e1)): loop(function er: {
					(e) with eexpr = TReturn(Some(er))
				}, e1);
			case TBinop(OpAssign | OpAssignOp(_) = op, { eexpr = TLocal(_) | TField(_) | TArray(_) } = e1, e2): loop(function er: {
							(e) with eexpr = TBinop(op, e1, er)
						}, e2);
			case TBlock(el): {
				(e) with eexpr = TBlock(block(el))
			};
			case _: Type.map_expr(find, e);
			};
		};
		find(e);
	};

	public static function add_final_return(e) return {
		function loop(e, t) return {
			function def_return(p) return {
				var c = switch (follow(t)) {
				case TAbstract({ a_path = ([], Int) }, _): TInt();
				case TAbstract({ a_path = ([], Float) }, _): TFloat("0.");
				case TAbstract({ a_path = ([], Bool) }, _): TBool(False);
				case _: TNull;
				};
				{
					() with eexpr = TReturn(Some({
						() with eexpr = TConst(c);
						epos = p;
						etype = t
					}));
					etype = t_dynamic;
					epos = p
				};
			};
			switch (e.eexpr) {
			case TBlock(el): switch (List.rev(el)) {
				case []: e;
				case ::(elast, el): switch (loop(elast, t)) {
					case {
							eexpr = TBlock(el2)
						}: {
							(e) with eexpr = TBlock(@(List.rev(el), el2))
						};
					case elast: {
						(e) with eexpr = TBlock(List.rev(::(elast, el)))
					};
					};
				};
			case TReturn(_): e;
			case _: {
					(e) with eexpr = TBlock(::(e, ::(def_return(e.epos), [])))
				};
			};
		};
		var e = Type.map_expr(add_final_return, e);
		switch (e.eexpr) {
		case TFunction(f): var f = switch (follow(f.tf_type)) {
			case TAbstract({ a_path = ([], Void) }, []): f;
			case _: {
				(f) with tf_expr = loop(f.tf_expr, f.tf_type)
			};
			};
			{
				(e) with eexpr = TFunction(f)
			};
		case _: e;
		};
	};

	public static function wrap_js_exceptions(com, e) return {
		function is_error(t) return {
			switch (follow(t)) {
			case TInst({ cl_path = (::(js, []), Error) }, _): True;
			case TInst({ cl_super = Some(csup, tl) }, _): is_error(TInst(csup, tl));
			case _: False;
			};
		};
		function loop(e) return {
			switch (e.eexpr) {
			case TThrow(eerr) if (!(is_error(eerr.etype))): var terr = List.find(function mt:
				switch (mt) {
			case TClassDecl({ cl_path = (::(js, ::(_Boot, [])), HaxeError) }): True;
				case _: False;
				}, com.types);
				var cerr = switch (terr) {
				case TClassDecl(c): c;
				case _: assert False;
				};
				var ewrap = { (eerr) with eexpr = TNew(cerr, [], ::(eerr, [])) };
				{
					(e) with eexpr = TThrow(ewrap)
				};
			case _: Type.map_expr(loop, e);
			};
		};
		loop(e);
	};

	public static function check_local_vars_init(e) return {
		function intersect(vl1, vl2) return {
			PMap.mapi(function v: function t: && (t, PMap.find(v, vl2)), vl1);
		};
		function join(vars, cvars) return {
			List.iter(function v: vars.val = intersect(vars.val, v), cvars);
		};
		function restore(vars, old_vars, declared) return {
			vars.val = List.fold_left(function acc: function v:
			try {
				PMap.add(v, PMap.find(v, old_vars), acc);
			} catch (e: Not_found) {
				PMap.remove(v, acc);
			}, vars.val, declared);
		};
		var declared = ref([]);
		function loop(vars, e) return {
			switch (e.eexpr) {
			case TLocal(v): var init = try {
					PMap.find(v.v_id, vars.val);
				} catch (e: Not_found) {
					True;
				};
				if (!(init)) {
					if ( = (v.v_name, "this")) {
						error("Missing this = value", e.epos);
					} else {
						error( ^ ("Local variable ", ^ (v.v_name, " used without being initialized")), e.epos);
					};
				} else {
					[];
				};
			case TVar(v, eo): switch (eo) {
				case None: declared.val = ::(v.v_id, declared.val);
					vars.val = PMap.add(v.v_id, False, vars.val);
				case Some(e): loop(vars, e);
				};
			case TBlock(el): var old = declared.val;
				var old_vars = vars.val;
				declared.val = [];
				List.iter(loop(vars), el);
				restore(vars, old_vars, List.rev(declared.val));
				declared.val = old;
			case TBinop(OpAssign, { eexpr = TLocal(v) }, e) if (PMap.mem(v.v_id, vars.val)): loop(vars, e);
				vars.val = PMap.add(v.v_id, True, vars.val);
			case TIf(e1, e2, eo): loop(vars, e1);
				var vbase = vars.val;
				loop(vars, e2);
				switch (eo) {
				case None: vars.val = vbase;
				case Some({ eexpr = TConst(TBool(False)) }): [];
				case Some(e): var v1 = vars.val;
					vars.val = vbase;
					loop(vars, e);
					vars.val = intersect(vars.val, v1);
				};
			case TWhile(cond, e, flag): switch (flag) {
				case NormalWhile if (switch (cond.eexpr) {
						case TParenthesis({ eexpr = TConst(TBool(True)) }): False;
							case _: True;
							}): loop(vars, cond);
					var old = vars.val;
					loop(vars, e);
					vars.val = old;
				case _: loop(vars, e);
					loop(vars, cond);
				};
			case TTry(e, catches): var cvars = List.map(function (v, e): var old = vars.val;
												   loop(vars, e);
												   var v = vars.val;
												   vars.val = old;
												   v, catches);
				loop(vars, e);
				join(vars, cvars);
			case TSwitch(e, cases, def): loop(vars, e);
				var cvars = List.map(function (ec, e): var old = vars.val;
									 List.iter(loop(vars), ec);
									 vars.val = old;
									 loop(vars, e);
									 var v = vars.val;
									 vars.val = old;
									 v, cases);
				switch (def) {
				case None if (switch (e.eexpr) {
						case TMeta((Meta.Exhaustive, _, _), _) | TParenthesis({ eexpr = TMeta((Meta.Exhaustive, _, _), _) }): True;
							case _: False;
							}):
					switch (cvars) {
					case ::(cv, cvars): PMap.iter(function i: function b: if (b) {
						vars.val = PMap.add(i, b, vars.val);
						} else {
							[];
						}, cv);
						join(vars, cvars);
					case []: [];
					};
				case None: [];
				case Some(e): loop(vars, e);
					join(vars, cvars);
				};
			case TBreak | TContinue | TReturn(None): vars.val = PMap.map(function _: True, vars.val);
			case TThrow(e) | TReturn(Some(e)): loop(vars, e);
				vars.val = PMap.map(function _: True, vars.val);
			case TFunction(_): [];
			case _: Type.iter(loop(vars), e);
			};
		};
		loop(ref(PMap.empty), e);
		e;
	};

	public static function local_usage(f, e) return {
		switch (e.eexpr) {
		case TBinop(OpAssign | OpAssignOp(_), { eexpr = TLocal(v) }, e2): local_usage(f, e2);
			f(Assign(v));
		case TUnop(Increment | Decrement, _, { eexpr = TLocal(v) }): f(Assign(v));
		case TLocal(v): f(Use(v));
		case TVar(v, eo): switch (eo) {
			case None: [];
			case Some(e): local_usage(f, e);
			};
			f(Declare(v));
		case TFunction(tf): 	function cc(f) return {
				List.iter(function (v, _): f(Declare(v)), tf.tf_args);
				local_usage(f, tf.tf_expr);
			};
			f(Function(cc));
		case TBlock(l): f(Block(function f: List.iter(local_usage(f), l)));
		case TFor(v, it, e): local_usage(f, it);
			f(Loop(function f: f(Declare(v));
				   local_usage(f, e)));
		case TWhile(_): f(Loop(function f: iter(local_usage(f), e)));
		case TTry(e, catchs): local_usage(f, e);
			List.iter(function (v, e): f(Block(function f: f(Declare(v));
											   local_usage(f, e))), catchs);
		case _: iter(local_usage(f), e);
		};
	};

	public static function captured_vars(com, e) return {
		var t = com.basic;
		var impl = switch (com.platform) {
		case Cs | Java: var cnativearray = switch (List.find(function md:
			switch (md) {
			case TClassDecl({ cl_path = (::(cs | java, []), NativeArray) }): True;
				case _: False;
				}, com.types)) {
			case TClassDecl(cl): cl;
			case _: assert False;
			};
			method(!) ? (private) ? s : t = e
											or method(!) ? (private) ? s = ecaptured_typenullnullfunction t : TInst(cnativearray, ::(t, []));
			method(!) ? (private) ? s : t = e or method(!) ? (private) ? s = emk_refnullnullfunction v : function ve : function p :
			switch (ve) {
			case None: var eone = mk(TConst(TInt(Int32.of_int(1))), t.tint, p);
				var t = switch (v.v_type) {
				case TInst(_, ::(t, [])): t;
				case _: assert False;
				};
				mk(TNew(cnativearray, ::(t, []), ::(eone, [])), v.v_type, p);
			case Some(e): {
				(Optimizer.mk_untyped_call("__array__", p, ::(e, []))) with etype = v.v_type
			};
			};
			method(!) ? (private) ? s : t = e or method(!) ? (private) ? s = emk_ref_accessnullnullfunction e : function v : mk(TArray({ (e) with etype = v.v_type },
											mk(TConst(TInt()), t.tint, e.epos)), e.etype, e.epos);
			method(!) ? (private) ? s : t = e
											or method(!) ? (private) ? s = emk_initnullnullfunction av : function v : function pos : var elocal = mk(TLocal(v),
													v.v_type, pos);
			var earray = { (Optimizer.mk_untyped_call("__array__", pos, ::(elocal, []))) with etype = av.v_type };
			mk(TVar(av, Some(earray)), t.tvoid, pos);
		case _: method(!) ? (private) ? s : t = e or method(!) ? (private) ? s = ecaptured_typenullnullt.tarray;
			method(!) ? (private) ? s : t = e
			or method(!) ? (private) ? s = emk_refnullnullfunction v : function ve : function p : mk(TArrayDecl(switch (ve) {
		case None: [];
			case Some(e): ::(e, []);
			}), v.v_type, p);
			method(!) ? (private) ? s : t = e or method(!) ? (private) ? s = emk_ref_accessnullnullfunction e : function v : mk(TArray({ (e) with etype = v.v_type },
											mk(TConst(TInt()), t.tint, e.epos)), e.etype, e.epos);
			method(!) ? (private) ? s : t = e
											or method(!) ? (private) ? s = emk_initnullnullfunction av : function v : function pos : mk(TVar(av,
													Some(mk(TArrayDecl(::(mk(TLocal(v), v.v_type, pos), [])), av.v_type, pos))), t.tvoid, pos);
		};
		function mk_var(v, used) return {
			var v2 = alloc_var(v.v_name, PMap.find(v.v_id, used));
			v2.v_meta = v.v_meta;
			v2;
		};
		function wrap(used, e) return {
			switch (e.eexpr) {
			case TVar(v, ve): var Tuple(v, ve) = if (PMap.mem(v.v_id, used)) {
					(new Tuple(v, Some(implmk_ref(v, Option.map(wrap(used), ve), e.epos))));
				} else {
					(new Tuple(v, switch (ve) {
				case None: None;
				case Some(e): Some(wrap(used, e));
					}));
				};
				{
					(e) with eexpr = TVar(v, ve)
				};
			case TLocal(v) if (PMap.mem(v.v_id, used)): implmk_ref_access(e, v);
			case TFor(v, it, expr) if (PMap.mem(v.v_id, used)): var vtmp = mk_var(v, used);
				var it = wrap(used, it);
				var expr = wrap(used, expr);
				mk(TFor(vtmp, it, Type.concat(implmk_init(v, vtmp, e.epos), expr)), e.etype, e.epos);
			case TTry(expr, catchs): var catchs = List.map(function (v, e): var e = wrap(used, e);
				try {
					var vtmp = mk_var(v, used);
					(new Tuple(vtmp, Type.concat(implmk_init(v, vtmp, e.epos), e)));
				} catch (e: Not_found) {
					(new Tuple(v, e));
				}, catchs);
				mk(TTry(wrap(used, expr), catchs), e.etype, e.epos);
			case TFunction(f): var fused = ref(PMap.empty);
				var tmp_used = ref(used);
				function browse(match) return switch (match) {
				case Block(f) | Loop(f) | Function(f): f(browse);
				case Use(v) | Assign(v): if (PMap.mem(v.v_id, tmp_used.val)) {
						fused.val = PMap.add(v.v_id, v, fused.val);
					} else {
						[];
					};
				case Declare(v): tmp_used.val = PMap.remove(v.v_id, tmp_used.val);
				};
				local_usage(browse, e);
				var vars = PMap.fold(function v: function acc: ::(v, acc), fused.val, []);
				var fexpr = ref(wrap(used, f.tf_expr));
				var fargs = List.map(function (v, o):
				if (PMap.mem(v.v_id, used)) {
				var vtmp = mk_var(v, used);
					fexpr.val = Type.concat(implmk_init(v, vtmp, e.epos), fexpr.val);
					(new Tuple(vtmp, o));
				} else {
					(new Tuple(v, o));
				}, f.tf_args);
				var e = { (e) with eexpr = TFunction({ (f) with tf_args = fargs;
													   tf_expr = fexpr.val
													 })
						};
				if ( = (com.config.pf_capture_policy, CPLoopVars)) {
					mk(TCall(Codegen.mk_parent(mk(TFunction({ () with tf_args = List.map(function v: (new Tuple(v, None)), vars);
															tf_type = e.etype;
															tf_expr = mk_block(mk(TReturn(Some(e)), e.etype, e.epos))
															}), TFun(List.map(function v: (new Tuple(v.v_name, False, v.v_type)), vars), e.etype), e.epos)),
							 List.map(function v: mk(TLocal(v), v.v_type, e.epos), vars)), e.etype, e.epos);
				} else {
					e;
				};
			case _: map_expr(wrap(used), e);
			};
		};
		function do_wrap(used, e) return {
			if (PMap.is_empty(used)) {
				e;
			} else {
				var used = PMap.map(function v: var vt = v.v_type;
				v.v_type = implcaptured_type(vt);
				v.v_capture = True;
				vt, used);
				wrap(used, e);
			};
		};
		function out_loop(e) return {
			switch (e.eexpr) {
			case TFor(_) | TWhile(_): var vars = ref(PMap.empty);
				var used = ref(PMap.empty);
				var depth = ref(0);
				function collect_vars(in_loop) return {
				case Block(f): var old = vars.val;
					f(collect_vars(in_loop));
					vars.val = old;
				case Loop(f): var old = vars.val;
					f(collect_vars(True));
					vars.val = old;
				case Function(f): incr(depth);
					f(collect_vars(False));
					decr(depth);
				case Declare(v): if (in_loop) {
						vars.val = PMap.add(v.v_id, depth.val, vars.val);
					} else {
						[];
					};
				case Use(v) | Assign(v): try {
						var d = PMap.find(v.v_id, vars.val);
						if (<>(d, depth.val)) {
							used.val = PMap.add(v.v_id, v, used.val);
						} else {
							[];
						};
					} catch (e: Not_found) {
						[];
					};
				};
				local_usage(collect_vars(False), e);
				do_wrap(used.val, e);
			case _: map_expr(out_loop, e);
			};
		};
		function all_vars(e) return {
			var vars = ref(PMap.empty);
			var used = ref(PMap.empty);
			var assigned = ref(PMap.empty);
			var depth = ref(0);
			function collect_vars(match) return switch (match) {
			case Block(f): var old = vars.val;
				f(collect_vars);
				vars.val = old;
			case Loop(f): var old = vars.val;
				f(collect_vars);
				vars.val = old;
			case Function(f): incr(depth);
				f(collect_vars);
				decr(depth);
			case Declare(v): vars.val = PMap.add(v.v_id, depth.val, vars.val);
			case Use(v): try {
					var d = PMap.find(v.v_id, vars.val);
					if (<>(d, depth.val)) {
						used.val = PMap.add(v.v_id, v, used.val);
					} else {
						[];
					};
				} catch (e: Not_found) {
					[];
				};
			case Assign(v): try {
					var d = PMap.find(v.v_id, vars.val);
					if (<>(d, depth.val)) {
						used.val = PMap.add(v.v_id, v, used.val);
						assigned.val = PMap.add(v.v_id, v, assigned.val);
					} else {
						if (PMap.mem(v.v_id, used.val)) {
							assigned.val = PMap.add(v.v_id, v, assigned.val);
						} else {
							[];
						};
					};
				} catch (e: Not_found) {
					[];
				};
			};
			local_usage(collect_vars, e);
			PMap.iter(function _: function v: v.v_capture = True, used.val);
			assigned.val;
		};
		var captured = all_vars(e);
		switch (com.config.pf_capture_policy) {
		case CPNone: e;
		case CPWrapRef: do_wrap(captured, e);
		case CPLoopVars: out_loop(e);
		};
	};

	public static function rename_local_vars(ctx, e) return {
		var cfg = ctx.com.config;
		var all_scope = || (!(cfg.pf_captured_scope), !(cfg.pf_locals_scope));
		var vars = ref(PMap.empty);
		var all_vars = ref(PMap.empty);
		var vtemp = alloc_var("~", t_dynamic);
		var rebuild_vars = ref(False);
		function rebuild(m) return {
			PMap.fold(function v: function acc: PMap.add(v.v_name, v, acc), m, PMap.empty);
		};
		function save([]) return {
			var old = vars.val;
			if ( || (cfg.pf_unique_locals, !(cfg.pf_locals_scope))) {
				function []: [];
			} else {
				function []: vars.val = if (rebuild_vars.val) {
					rebuild(old);
				} else {
					old;
				};
			};
		};
		function rename(vars, v) return {
			var count = ref(1);
			PMap.mem( ^ (v.v_name, string_of_int(count.val)), vars)incr(count);
			v.v_name = ^ (v.v_name, string_of_int(count.val));
		};
		function declare(v, p) return {
			switch (follow(v.v_type)) {
			case TAbstract({ a_path = ([], Void) }, _): error("Arguments and variables of type Void are not allowed", p);
			case _: [];
			};
			if (is_gen_local(v)) {
				v.v_name = ^ ("_g", String.sub(v.v_name, 1, -(String.length(v.v_name), 1)));
			} else {
				[];
			};
			var look_vars = if ( && (!(cfg.pf_captured_scope), v.v_capture)) {
				all_vars.val;
			} else {
				vars.val;
			};
			try {
				var v2 = PMap.find(v.v_name, look_vars);
				if ( == (v, v2)) {
					raise(Not_found);
				} else {
					[];
				};
				rename(look_vars, v);
			} catch (e: Not_found) {
				[];
			};
			vars.val = PMap.add(v.v_name, v, vars.val);
			if (all_scope) {
				all_vars.val = PMap.add(v.v_name, v, all_vars.val);
			} else {
				[];
			};
		};
		function check(t) return {
			switch (t_infos(t).mt_path) {
				([], name) | (::(name, _), _): var vars = if (cfg.pf_locals_scope) {
					vars;
				} else {
					all_vars;
				};
				try {
					var v = PMap.find(name, vars.val);
					if ( == (v, vtemp)) {
						raise(Not_found);
					} else {
						[];
					};
					rename(vars.val, v);
					rebuild_vars.val = True;
					vars.val = PMap.add(v.v_name, v, vars.val);
				} catch (e: Not_found) {
					[];
				};
				vars.val = PMap.add(name, vtemp, vars.val);
			};
		};
		function check_type(t) return {
			switch (follow(t)) {
			case TInst(c, _): check(TClassDecl(c));
			case TEnum(e, _): check(TEnumDecl(e));
			case TType(t, _): check(TTypeDecl(t));
			case TAbstract(a, _): check(TAbstractDecl(a));
			case TMono(_) | TLazy(_) | TAnon(_) | TDynamic(_) | TFun(_): [];
			};
		};
		function loop(e) return {
			switch (e.eexpr) {
			case TVar(v, eo): if (!(cfg.pf_locals_scope)) {
					declare(v, e.epos);
				} else {
					[];
				};
				switch (eo) {
				case None: [];
				case Some(e): loop(e);
				};
				if (cfg.pf_locals_scope) {
					declare(v, e.epos);
				} else {
					[];
				};
			case TFunction(tf): var old = save([]);
				List.iter(function (v, _): declare(v, e.epos), tf.tf_args);
				loop(tf.tf_expr);
				old([]);
			case TBlock(el): var old = save([]);
				switch (ctx.com.platform) {
				case Js: 	function check_var(e) return {
						switch (e.eexpr) {
						case TVar(v, eo): switch (eo) {
							case None: [];
							case Some(e): loop(e);
							};
							declare(v, e.epos);
						case TBlock(_): [];
						case _: Type.iter(check_var, e);
						};
					};
					List.iter(check_var, el);
				case _: [];
				};
				List.iter(loop, el);
				old([]);
			case TFor(v, it, e1): loop(it);
				var old = save([]);
				declare(v, e.epos);
				loop(e1);
				old([]);
			case TTry(e, catchs): loop(e);
				List.iter(function (v, e): var old = save([]);
						  declare(v, e.epos);
						  check_type(v.v_type);
						  loop(e);
						  old([]), catchs);
			case TTypeExpr(t): check(t);
			case TNew(c, _, _): Type.iter(loop, e);
				check(TClassDecl(c));
			case TCast(e, Some(t)): loop(e);
				check(t);
			case TConst(TSuper): check_type(e.etype);
			case _: Type.iter(loop, e);
			};
		};
		declare(alloc_var("this", t_dynamic), Ast.null_pos);
		switch (ctx.curclass.cl_path) {
			(::(s, _), _) | ([], s): declare(alloc_var(s, t_dynamic), Ast.null_pos);
		};
		loop(e);
		e;
	};

	public static function check_unification(ctx, e, t) return {
		switch ((new Tuple(e.eexpr, t))) {
		case (TLocal(v), TType({ t_path = (::(cs, []), Ref | Out) }, _)): v.v_capture = True;
		case _: [];
		};
		e;
	};

	public static function save_class_state(ctx, t) return {
		switch (t) {
		case TClassDecl(c): 	function mk_field_restore(f) return {
				function mk_overload_restore(f) return {
					(new Tuple(f.cf_name, f.cf_kind, f.cf_expr, f.cf_type, f.cf_meta, f.cf_params));
				};
				(new Tuple(f, mk_overload_restore(f), List.map(function f: (new Tuple(f, mk_overload_restore(f))), f.cf_overloads)));
			};
			function restore_field(Tuple(f, res, overloads)) return {
				function restore_field(Tuple(f, Tuple(name, kind, expr, t, meta, params))) return {
					f.cf_name = name;
					f.cf_kind = kind;
					f.cf_expr = expr;
					f.cf_type = t;
					f.cf_meta = meta;
					f.cf_params = params;
					f;
				};
				var f = restore_field((new Tuple(f, res)));
				f.cf_overloads = List.map(restore_field, overloads);
				f;
			};
			function mk_pmap(lst) return {
				List.fold_left(function pmap: function f: PMap.add(f.cf_name, f, pmap), PMap.empty, lst);
			};
			var meta = c.cl_meta;
			var path = c.cl_path;
			var ext = c.cl_extern;
			var over = c.cl_overrides;
			var sup = c.cl_super;
			var impl = c.cl_implements;
			var csr = Option.map(mk_field_restore, c.cl_constructor);
			var ofr = List.map(mk_field_restore, c.cl_ordered_fields);
			var osr = List.map(mk_field_restore, c.cl_ordered_statics);
			var init = c.cl_init;
			c.cl_restore = function []: c.cl_super = sup;
			c.cl_implements = impl;
			c.cl_meta = meta;
			c.cl_extern = ext;
			c.cl_path = path;
			c.cl_init = init;
			c.cl_ordered_fields = List.map(restore_field, ofr);
			c.cl_ordered_statics = List.map(restore_field, osr);
			c.cl_fields = mk_pmap(c.cl_ordered_fields);
			c.cl_statics = mk_pmap(c.cl_ordered_statics);
			c.cl_constructor = Option.map(restore_field, csr);
			c.cl_overrides = over;
		case _: [];
		};
	};

	public static function is_removable_class(c) return {
		switch (c.cl_kind) {
		case KGeneric: || (Meta.has(Meta.Remove, c.cl_meta), || (switch (c.cl_super) {
		case Some(c, _): is_removable_class(c);
			case _: False;
			}, List.exists(function (_, t):
			switch (follow(t)) {
		case TInst(c, _): || (Codegen.has_ctor_constraint(c), Meta.has(Meta.Const, c.cl_meta));
			case _: False;
			}, c.cl_params)));
		case KTypeParameter(_): True;
		case _: False;
		};
	};

	public static function remove_generic_base(ctx, t) return {
		switch (t) {
		case TClassDecl(c) if (is_removable_class(c)): c.cl_extern = True;
		case _: [];
		};
	};

	public static function remove_extern_fields(ctx, t) return {
		switch (t) {
		case TClassDecl(c): if (!(Common.defined(ctx.com, Define.DocGen))) {
				c.cl_ordered_fields = List.filter(function f: var b = Codegen.is_removable_field(ctx, f);
				if (b) {
				c.cl_fields = PMap.remove(f.cf_name, c.cl_fields);
				} else {
					[];
				};
				!(b), c.cl_ordered_fields);
				c.cl_ordered_statics = List.filter(function f: var b = Codegen.is_removable_field(ctx, f);
				if (b) {
				c.cl_statics = PMap.remove(f.cf_name, c.cl_statics);
				} else {
					[];
				};
				!(b), c.cl_ordered_statics);
			} else {
				[];
			};
		case _: [];
		};
	};

	public static function check_private_path(ctx, t) return {
		switch (t) {
		case TClassDecl(c) if (c.cl_private): var rpath = (new Tuple(fst(c.cl_module.m_path), ^ ("_", snd(c.cl_module.m_path))));
			if (Hashtbl.mem(ctx.g.types_module, rpath)) {
				error( ^ ("This private class name will clash with ", s_type_path(rpath)), c.cl_pos);
			} else {
				[];
			};
		case _: [];
		};
	};

	public static function apply_native_paths(ctx, t) return {
		function get_native_name(meta) return {
			function get_native(meta) return {
				switch (meta) {
				case []: raise(Not_found);
				case ::((Meta.Native, ::(v, []), p) = meta, _): meta;
				case ::(_, meta): get_native(meta);
				};
			};
			var Tuple(_, e, mp) = get_native(meta);
			switch (e) {
			case ::((Ast.EConst(Ast.String(name)), p), []): (new Tuple(name, p));
			case []: raise(Not_found);
			case _: error("String expected", mp);
			};
		};
		function get_real_name(meta, name) return {
			var Tuple(name', p) = get_native_name(meta);
			(new Tuple((new Tuple(Meta.RealPath, ::((new Tuple(Ast.EConst(Ast.String(name)), p)), []), p)), name'));
		};
		function get_real_path(meta, path) return {
			var Tuple(name, p) = get_native_name(meta);
			(new Tuple((new Tuple(Meta.RealPath, ::((new Tuple(Ast.EConst(Ast.String(s_type_path(path))), p)), []), p)), parse_path(name)));
		};
		try {
			switch (t) {
			case TClassDecl(c): var did_change = ref(False);
				function field(cf) return {
					try {
						var Tuple(meta, name) = get_real_name(cf.cf_meta, cf.cf_name);
						cf.cf_name = name;
						cf.cf_meta = ::(meta, cf.cf_meta);
						List.iter(function cf: cf.cf_name = name, cf.cf_overloads);
						did_change.val = True;
					} catch (e: Not_found) {
						[];
					};
				};
				function fields(cfs, old_map) return {
					did_change.val = False;
					List.iter(field, cfs);
					if (did_change.val) {
						List.fold_left(function map: function f: PMap.add(f.cf_name, f, map), PMap.empty, cfs);
					} else {
						old_map;
					};
				};
				c.cl_fields = fields(c.cl_ordered_fields, c.cl_fields);
				c.cl_statics = fields(c.cl_ordered_statics, c.cl_statics);
				var Tuple(meta, path) = get_real_path(c.cl_meta, c.cl_path);
				c.cl_meta = ::(meta, c.cl_meta);
				c.cl_path = path;
			case TEnumDecl(e): var Tuple(meta, path) = get_real_path(e.e_meta, e.e_path);
				e.e_meta = ::(meta, e.e_meta);
				e.e_path = path;
			case TAbstractDecl(a) if (Meta.has(Meta.CoreType, a.a_meta)): var Tuple(meta, path) = get_real_path(a.a_meta, a.a_path);
				a.a_meta = ::(meta, a.a_meta);
				a.a_path = path;
			case _: [];
			};
		} catch (e: Not_found) {
			[];
		};
	};

	public static function add_rtti(ctx, t) return {
		function has_rtti(c) return {
			|| (Meta.has(Meta.Rtti, c.cl_meta), switch (c.cl_super) {
		case None: False;
		case Some(csup, _): has_rtti(csup);
			});
		};
		switch (t) {
		case TClassDecl(c) if (&&(has_rtti(c), !(PMap.mem("__rtti", c.cl_statics)))): var f = mk_field("__rtti", ctx.t.tstring,
					c.cl_pos);
			var str = Genxml.gen_type_string(ctx.com, t);
			f.cf_expr = Some(mk(TConst(TString(str)), f.cf_type, c.cl_pos));
			c.cl_ordered_statics = ::(f, c.cl_ordered_statics);
			c.cl_statics = PMap.add(f.cf_name, f, c.cl_statics);
		case _: [];
		};
	};

	public static function add_field_inits(ctx, t) return {
		var is_as3 = && (Common.defined(ctx.com, Define.As3), !(ctx.in_macro));
		function apply(c) return {
			var ethis = mk(TConst(TThis), TInst(c, List.map(snd, c.cl_params)), c.cl_pos);
			var v = alloc_var("_g", ethis.etype);
			var need_this = ref(False);
			var Tuple(inits, fields) = List.fold_left(function (inits, fields): function cf:
			switch ((new Tuple(cf.cf_kind, cf.cf_expr))) {
		case (Var(_), Some(_)): if (is_as3) {
					(new Tuple(inits, ::(cf, fields)));
				} else {
					(new Tuple(::(cf, inits), ::(cf, fields)));
				};
			case (Method(MethDynamic), Some(e)) if (is_as3): 	function use_this(v, e) return {
					switch (e.eexpr) {
					case TConst(TThis): need_this.val = True;
						mk(TLocal(v), v.v_type, e.epos);
					case _: Type.map_expr(use_this(v), e);
					};
				};
				var e = Type.map_expr(use_this(v), e);
				var cf2 = { (cf) with cf_expr = Some(e) };
				var fields = if (List.memq(cf, c.cl_overrides)) {
					c.cl_fields = PMap.remove(cf.cf_name, c.cl_fields);
					fields;
				} else {
					::(cf2, fields);
				};
				(new Tuple(::(cf2, inits), fields));
			case _: (new Tuple(inits, ::(cf, fields)));
			}, (new Tuple([], [])), c.cl_ordered_fields);
			c.cl_ordered_fields = List.rev(fields);
			switch (inits) {
			case []: [];
			case _: var el = List.map(function cf: switch (cf.cf_expr) {
			case None: assert False;
			case Some(e): var lhs = mk(TField(ethis, FInstance(c, List.map(snd, c.cl_params), cf)), cf.cf_type, e.epos);
					cf.cf_expr = None;
					var eassign = mk(TBinop(OpAssign, lhs, e), e.etype, e.epos);
					if (is_as3) {
						var echeck = mk(TBinop(OpEq, lhs, mk(TConst(TNull), lhs.etype, e.epos)), ctx.com.basic.tbool, e.epos);
						mk(TIf(echeck, eassign, None), eassign.etype, e.epos);
					} else {
						eassign;
					};
				}, inits);
				var el = if (need_this.val) {
					::(mk(TVar(v, Some(ethis)), ethis.etype, ethis.epos), el);
				} else {
					el;
				};
				switch (c.cl_constructor) {
				case None: var ct = TFun([], ctx.com.basic.tvoid);
					var ce = mk(TFunction({ () with tf_args = [];
											tf_type = ctx.com.basic.tvoid;
											tf_expr = mk(TBlock(el), ctx.com.basic.tvoid, c.cl_pos)
										  }), ct, c.cl_pos);
					var ctor = mk_field("new", ct, c.cl_pos);
					ctor.cf_kind = Method(MethNormal);
					c.cl_constructor = Some({ (ctor) with cf_expr = Some(ce) });
				case Some(cf): switch (cf.cf_expr) {
					case Some({ eexpr = TFunction(f) }): var bl = switch (f.tf_expr) {
						case {
								eexpr = TBlock(b)
							}: b;
						case x: ::(x, []);
						};
						var ce = mk(TFunction({ (f) with tf_expr = mk(TBlock(@(el, bl)), ctx.com.basic.tvoid, c.cl_pos) }), cf.cf_type, cf.cf_pos);
						c.cl_constructor = Some({ (cf) with cf_expr = Some(ce) });
					case _: assert False;
					};
				};
			};
		};
		switch (t) {
		case TClassDecl(c): apply(c);
		case _: [];
		};
	};

	public static function add_meta_field(ctx, t) return {
		switch (t) {
		case TClassDecl(c): switch (Codegen.build_metadata(ctx.com, t)) {
			case None: [];
			case Some(e): var f = mk_field("__meta__", t_dynamic, c.cl_pos);
				f.cf_expr = Some(e);
				function can_deal_with_interface_metadata([]) return {
					switch (ctx.com.platform) {
					case Flash if (Common.defined(ctx.com, Define.As3)): False;
					case Php: False;
					case _: True;
					};
				};
				if ( && (c.cl_interface, !(can_deal_with_interface_metadata([])))) {
					var path = (new Tuple(fst(c.cl_path), ^ (snd(c.cl_path), "_HxMeta")));
					var ncls = mk_class(c.cl_module, path, c.cl_pos);
					var cf = mk_field("__meta__", e.etype, e.epos);
					cf.cf_expr = Some(e);
					ncls.cl_statics = PMap.add("__meta__", cf, ncls.cl_statics);
					ncls.cl_ordered_statics = ::(cf, ncls.cl_ordered_statics);
					ctx.com.types = ::(TClassDecl(ncls), ctx.com.types);
					c.cl_meta = ::((new Tuple(Meta.Custom(":hasMetadata"), [], e.epos)), c.cl_meta);
				} else {
					c.cl_ordered_statics = ::(f, c.cl_ordered_statics);
					c.cl_statics = PMap.add(f.cf_name, f, c.cl_statics);
				};
			};
		case _: [];
		};
	};

	public static function check_remove_metadata(ctx, t) return {
		switch (t) {
		case TClassDecl(c): c.cl_implements = List.filter(function (c, _): !(Meta.has(Meta.Remove, c.cl_meta)), c.cl_implements);
		case _: [];
		};
	};

	public static function check_void_field(ctx, t) return {
		switch (t) {
		case TClassDecl(c): 	function check(f) return {
				switch (follow(f.cf_type)) {
				case TAbstract({ a_path = ([], Void) }, _): error("Fields of type Void are not allowed", f.cf_pos);
				case _: [];
				};
			};
			List.iter(check, c.cl_ordered_fields);
			List.iter(check, c.cl_ordered_statics);
		case _: [];
		};
	};

	public static function promote_first_interface_to_super(ctx, t) return {
		switch (t) {
		case TClassDecl(c) if (c.cl_interface):
			switch (c.cl_implements) {
			case ::(( {
					cl_path = (::(cpp, ::(rtti, [])), _)
				}, _), _): [];
			case ::(first_interface, remaining): c.cl_super = Some(first_interface);
				c.cl_implements = remaining;
			case _: [];
			};
		case _: [];
		};
	};

	public static function commit_features(ctx, t) return {
		var m = t_infos(t).mt_module;
		Hashtbl.iter(function k: function v: Common.add_feature(ctx.com, k), m.m_extra.m_features);
	};

	public static function check_reserved_type_paths(ctx, t) return {
		function check(path, pos) return {
			if (List.mem(path, ctx.com.config.pf_reserved_type_paths)) {
				ctx.com.warning( ^ ("Type path ", ^ (s_type_path(path), " is reserved on this target")), pos);
			} else {
				[];
			};
		};
		switch (t) {
		case TClassDecl(c) if (!(c.cl_extern)): check(c.cl_path, c.cl_pos);
		case TEnumDecl(e) if (!(e.e_extern)): check(e.e_path, e.e_pos);
		case _: [];
		};
	};

	public static function run_expression_filters(ctx, filters, t) return {
		function run(e) return {
			List.fold_left(function e: function f: f(e), e, filters);
		};
		switch (t) {
		case TClassDecl(c) if (is_removable_class(c)): [];
		case TClassDecl(c): ctx.curclass = c;
			function process_field(f) return {
				switch (f.cf_expr) {
				case Some(e) if (!(Codegen.is_removable_field(ctx, f))): Codegen.AbstractCast.cast_stack.val = ::(f,
					Codegen.AbstractCast.cast_stack.val);
					f.cf_expr = Some(run(e));
					Codegen.AbstractCast.cast_stack.val = List.tl(Codegen.AbstractCast.cast_stack.val);
				case _: [];
				};
				List.iter(process_field, f.cf_overloads);
			};
			List.iter(process_field, c.cl_ordered_fields);
			List.iter(process_field, c.cl_ordered_statics);
			switch (c.cl_constructor) {
			case None: [];
			case Some(f): process_field(f);
			};
			switch (c.cl_init) {
			case None: [];
			case Some(e): c.cl_init = Some(run(e));
			};
		case TEnumDecl(_): [];
		case TTypeDecl(_): [];
		case TAbstractDecl(_): [];
		};
	};

	public static var pp_counter = ref(1);

	public static function is_cached(t) return {
		var m = t_infos(t).mt_module.m_extra;
		if ( = (m.m_processed, 0)) {
			m.m_processed = pp_counter.val;
		} else {
			[];
		};
		<>(m.m_processed, pp_counter.val);
	};

	public static function apply_filters_once(ctx, filters, t) return {
		if (!(is_cached(t))) {
			run_expression_filters(ctx, filters, t);
		} else {
			[];
		};
	};

	public static function next_compilation([]) return {
		incr(pp_counter);
	};

	public static function iter_expressions(fl, mt) return {
		switch (mt) {
		case TClassDecl(c): 	function field(cf) return {
				switch (cf.cf_expr) {
				case None: [];
				case Some(e): List.iter(function f: f(e), fl);
				};
			};
			List.iter(field, c.cl_ordered_statics);
			List.iter(field, c.cl_ordered_fields);
			switch (c.cl_constructor) {
			case None: [];
			case Some(cf): field(cf);
			};
		case _: [];
		};
	};

	public static function run(com, tctx, main) return {
		switch (com.display) {
		case DMUsage | DMPosition: Codegen.detect_usage(com);
		case _: [];
		};
		if (!(Common.defined(com, Define.NoDeprecationWarnings))) {
			Codegen.DeprecationCheck.run(com);
		} else {
			[];
		};
		var use_static_analyzer = Common.defined(com, Define.Analyzer);
		var new_types = List.filter(function t: !(is_cached(t)), com.types);
		if (use_static_analyzer) {
			var filters = ::(Codegen.AbstractCast.handle_abstract_casts(tctx), ::(check_local_vars_init,
							 ::(Optimizer.inline_constructors(tctx), ::(Optimizer.reduce_expression(tctx), ::(blockify_ast,
									 ::(captured_vars(com), []))))));
			List.iter(run_expression_filters(tctx, filters), new_types);
			Analyzer.Run.run_on_types(tctx, new_types);
			List.iter(iter_expressions(::(verify_ast(tctx), [])), new_types);
			var filters = ::(Optimizer.sanitize(com), ::(if (com.config.pf_add_final_return) {
			add_final_return;
		} else {
			function e: e;
		}, ::(if ( = (com.platform, Js)) {
			wrap_js_exceptions(com);
			} else {
				function e: e;
			}, ::(rename_local_vars(tctx), []))));
			List.iter(run_expression_filters(tctx, filters), new_types);
		} else {
			var filters = ::(Codegen.AbstractCast.handle_abstract_casts(tctx), ::(blockify_ast, ::(check_local_vars_init, ::(Optimizer.inline_constructors(tctx), ::(if ( || (Common.defined(com, Define.NoSimplify), || (Common.defined(com, Define.Cppia), switch (com.platform) {
			case Cpp: False;
			case _: True;
			}))) {
			function e: e;
		} else {
			function e: var save = save_locals(tctx);
				var timer = timer("analyzer-simplify-apply");
				var e = try {
					snd(Analyzer.Simplifier.apply(com, e));
				} catch (e: Exit) {
					e;
				};
				timer([]);
				save([]);
				e;
			}, ::(if (com.foptimize) {
			function e: Optimizer.reduce_expression(tctx, e);
			} else {
				Optimizer.sanitize(com);
			}, ::(captured_vars(com), ::(promote_complex_rhs(com), ::(if (com.config.pf_add_final_return) {
			add_final_return;
		} else {
			function e: e;
		}, ::(if ( = (com.platform, Js)) {
			wrap_js_exceptions(com);
			} else {
				function e: e;
			}, ::(rename_local_vars(tctx), [])))))))))));
			List.iter(run_expression_filters(tctx, filters), new_types);
			List.iter(iter_expressions(::(verify_ast(tctx), [])), new_types);
		};
		next_compilation([]);
		List.iter(function f: f([]), List.rev(com.filters));
		List.iter(save_class_state(tctx), new_types);
		List.iter(function t: remove_generic_base(tctx, t);
				  remove_extern_fields(tctx, t), com.types);
		Codegen.update_cache_dependencies(com);
		List.iter(check_remove_metadata(tctx), com.types);
		var dce_mode = if (Common.defined(com, Define.As3)) {
			"no";
		} else {
			try {
				Common.defined_value(com, Define.Dce);
			} catch (e: _) {
				"no";
			};
		};
		switch (dce_mode) {
		case full: Dce.run(com, main, !(Common.defined(com, Define.Interp)));
		case std: Dce.run(com, main, False);
		case no: Dce.fix_accessors(com);
		case _: failwith( ^ ("Unknown DCE mode ", dce_mode));
		};
		List.iter(function mt:
		switch (mt) {
	case TClassDecl({ cl_kind = KAbstractImpl(_) } = c) if (&&(=(c.cl_ordered_statics, []), &&(=(c.cl_ordered_fields, []), !(Meta.has(Meta.Used, c.cl_meta)))))
					: c.cl_extern = True;
		case TClassDecl({ cl_kind = KAbstractImpl(a) } = c) if (Meta.has(Meta.Enum, a.a_meta)): 	function is_runtime_field(
				cf) return {
				!(Meta.has(Meta.Enum, cf.cf_meta));
			};
			if (!(List.exists(is_runtime_field, c.cl_ordered_statics))) {
				c.cl_extern = True;
			} else {
				[];
			};
		case _: [];
		}, com.types);
		var type_filters = ::(check_private_path, ::(apply_native_paths, ::(add_rtti, ::(switch (com.platform) {
	case Java | Cs: function _: function _: [];
		case _: add_field_inits;
		}, ::(add_meta_field, ::(check_void_field, ::(switch (com.platform) {
	case Cpp: promote_first_interface_to_super;
	case _: function _: function _: [];
		}, ::(commit_features, ::(if (<>(com.config.pf_reserved_type_paths, [])) {
		check_reserved_type_paths;
	} else {
		function _: function _: [];
		}, [])))))))));
		List.iter(function t: List.iter(function f: f(tctx, t), type_filters), com.types);
	}
}
;
