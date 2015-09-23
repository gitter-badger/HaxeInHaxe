import Ast;
import Type;
import Nast;
import Common;

typedef Context = {
	version : Int,
	com : Common.Context,
	packages : Hashtbl<String, Unit>,
	globals : Hashtbl<Tuple<List<String>, String>, String>,
	curglobal : Int,
	macros : Bool,
	curclass : String,
	curmethod : String,
	inits : List<Tuple<Tclass, Texpr>>,
	label_count : Int
};

class Genneko {
	public static var files = Hashtbl.create(0);

	public static function pos(ctx, p) return {
		if (ctx.macros) {
			{
				() with psource = p.pfile;
				pline = lor(p.pmin, lsl(-(p.pmax, p.pmin), 20))
			};
		} else {
			var file = switch (ctx.com.debug) {
			case True: ^ (ctx.curclass, ^ ("::", ctx.curmethod));
			case False: try {
					Hashtbl.find(files, p.pfile);
				} catch (e: Not_found) {
					var path = switch (Common.defined(ctx.com, Common.Define.AbsolutePath)) {
					case True: if (Filename.is_relative(p.pfile)) {
							Filename.concat(Sys.getcwd([]), p.pfile);
						} else {
							p.pfile;
						};
					case False: try {
							var len = String.length(p.pfile);
							var base = List.find(function path: var l = String.length(path);
												 && ( > (len, l), = (String.sub(p.pfile, 0, l), path)), ctx.com.Common.class_path);
							var l = String.length(base);
							String.sub(p.pfile, l, -(len, l));
						} catch (e: Not_found) {
							p.pfile;
						};
					};
					Hashtbl.add(files, p.pfile, path);
					path;
				};
			};
			{
				() with psource = file;
				pline = Lexer.get_error_line(p)
			};
		};
	};

	public static function gen_global_name(ctx, path) return {
		switch (path) {
		case ([], name): name;
		case _: try {
				Hashtbl.find(ctx.globals, path);
			} catch (e: Not_found) {
				var name = ^ ("@G", string_of_int(ctx.curglobal));
				ctx.curglobal = +(ctx.curglobal, 1);
				Hashtbl.add(ctx.globals, path, name);
				name;
			};
		};
	};

	public static function null(p) return {
		(new Tuple(EConst(Null), p));
	};

	public static function this(p) return {
		(new Tuple(EConst(This), p));
	};

	public static function int(p, n) return {
		(new Tuple(EConst(Int(n)), p));
	};

	public static function str(p, s) return {
		(new Tuple(EConst(String(s)), p));
	};

	public static function ident(p, s) return {
		var l = String.length(s);
		if ( && ( > (l, 10), = (String.sub(s, 0, 10), "__dollar__"))) {
			(new Tuple(EConst(Builtin(String.sub(s, 10, -(l, 10)))), p));
		} else {
			(new Tuple(EConst(Ident(s)), p));
		};
	};

	public static function field(p, e, f) return {
		(new Tuple(EField(e, f), p));
	};

	public static function builtin(p, n) return {
		(new Tuple(EConst(Builtin(n)), p));
	};

	public static function call(p, e, el) return {
		(new Tuple(ECall(e, el), p));
	};

	public static function array(p, el) return {
		call(p, builtin(p, "array"), el);
	};

	public static function pmap_list(f, p) return {
		PMap.fold(function v: function acc: ::(f(v), acc), p, []);
	};

	public static function needs_return(e) return {
		switch (e) {
		case (EBlock(l), _): function loop(match) return switch (match) {
			case []: True;
			case ::(x, []): needs_return(x);
			case ::(_, l): loop(l);
			};
			loop(l);
		case (EReturn(_), _): False;
		case _: True;
		};
	};

	public static function with_return(e) return {
		if (needs_return(e)) {
			var p = snd(e);
			var ret = (new Tuple(EReturn(Some(null(p))), p));
			switch (e) {
			case (EBlock(l), _): (new Tuple(EBlock(@(l, ::(ret, []))), p));
			case _: (new Tuple(EBlock(::(e, ::(ret, []))), p));
			};
		} else {
			e;
		};
	};

	public static function gen_type_path(p, Tuple(path, t)) return {
		switch (path) {
		case []: ident(p, t);
		case ::(path, l): var epath = List.fold_left(function e: function path: field(p, e, path), ident(p, path), l);
			field(p, epath, t);
		};
	};

	public static function gen_big_string(ctx, p, s) return {
		var max = -(lsl(1, 16), 1);
		if ( > (String.length(s), max)) {
			(new Tuple(EBinop("+", str(p, String.sub(s, 0, max)), gen_big_string(ctx, p, String.sub(s, max, -(String.length(s),
			max)))), p));
		} else {
			str(p, s);
		};
	};

	public static function gen_constant(ctx, pe, c) return {
		var p = pos(ctx, pe);
		switch (c) {
		case TInt(i): try {
				var h = Int32.to_int(Int32.shift_right_logical(i, 24));
				if (<>( = (land(h, 128), 0), = (land(h, 64), 0))) {
					raise(Exit);
				} else {
					[];
				};
				int(p, Int32.to_int(i));
			} catch (e: _) {
				if ( < (ctx.version, 2)) {
					error("This integer is too big to be compiled to a Neko 31-bit integer. Please use a Float instead", pe);
				} else {
					[];
				};
				(new Tuple(EConst(Int32(i)), p));
			};
		case TFloat(f): (new Tuple(EConst(Float(f)), p));
		case TString(s): call(p, field(p, ident(p, "String"), "new"), ::(gen_big_string(ctx, p, s), []));
		case TBool(b): (new Tuple(EConst(if (b) {
			True;
		} else {
			False;
		}), p));
		case TNull: null(p);
		case TThis: this(p);
		case TSuper: assert False;
		};
	};

	public static function gen_binop(ctx, p, op, e1, e2) return {
		(new Tuple(EBinop(Ast.s_binop(op), gen_expr(ctx, e1), gen_expr(ctx, e2)), p));
	};

	public static function gen_unop(ctx, p, op, flag, e) return {
		switch (op) {
		case Increment: (new Tuple(EBinop(if ( = (flag, Prefix)) {
			"+=";
		} else {
			"++=";
		}, gen_expr(ctx, e), int(p, 1)), p));
		case Decrement: (new Tuple(EBinop(if ( = (flag, Prefix)) {
			"-=";
		} else {
			"--=";
		}, gen_expr(ctx, e), int(p, 1)), p));
		case Not: call(p, builtin(p, "not"), ::(gen_expr(ctx, e), []));
		case Neg: (new Tuple(EBinop("-", int(p, 0), gen_expr(ctx, e)), p));
		case NegBits: (new Tuple(EBinop("-", int(p, -1), gen_expr(ctx, e)), p));
		};
	};

	public static function gen_call(ctx, p, e, el) return {
		switch ((new Tuple(e.eexpr, el))) {
		case (TConst(TSuper), _): var c = switch (follow(e.etype)) {
			case TInst(c, _): c;
			case _: assert False;
			};
			call(p, builtin(p, "call"), ::(field(p, gen_type_path(p, c.cl_path), "__construct__"), ::(this(p), ::(array(p,
										   List.map(gen_expr(ctx), el)), []))));
		case (TLocal({ v_name = __resources__ }), []): call(p, builtin(p, "array"),
					Hashtbl.fold(function name: function data: function acc: ::((new Tuple(EObject(::((new Tuple("name", gen_constant(ctx,
								 e.epos, TString(name)))), ::((new Tuple("data", gen_big_string(ctx, p, data))), []))), p)), acc), ctx.com.resources, []));
		case (TField({ eexpr = TConst(TSuper); etype = t }, f), _): var c = switch (follow(t)) {
			case TInst(c, _): c;
			case _: assert False;
			};
			call(p, builtin(p, "call"), ::(field(p, gen_type_path(p, (new Tuple(fst(c.cl_path), ^ ("@", snd(c.cl_path))))),
												 field_name(f)), ::(this(p), ::(array(p, List.map(gen_expr(ctx), el)), []))));
		case (_, _): var e = switch (gen_expr(ctx, e)) {
			case (EFunction(_), _) = e: (new Tuple(EBlock(::(e, [])), p));
			case e: e;
			};
			call(p, e, List.map(gen_expr(ctx), el));
		};
	};

	public static function gen_expr(ctx, e) return {
		var p = pos(ctx, e.epos);
		switch (e.eexpr) {
		case TConst(c): gen_constant(ctx, e.epos, c);
		case TLocal(v) if (=(v.v_name0, '$')): (new Tuple(EConst(Builtin(String.sub(v.v_name, 1, -(String.length(v.v_name), 1)))),
			p));
		case TLocal(v): if (v.v_capture) {
				(new Tuple(EArray(ident(p, v.v_name), int(p, 0)), p));
			} else {
				ident(p, v.v_name);
			};
		case TArray(e1, e2): (new Tuple(EArray(gen_expr(ctx, e1), gen_expr(ctx, e2)), p));
		case TBinop(OpAssign, { eexpr = TField(e1, f) }, e2): (new Tuple(EBinop("=", field(p, gen_expr(ctx, e1), field_name(f)),
					gen_expr(ctx, e2)), p));
		case TBinop(op, e1, e2): gen_binop(ctx, p, op, e1, e2);
		case TField(e2, FClosure(_, f)): switch (follow(e.etype)) {
			case TFun(args, _): var n = List.length(args);
				if ( > (n, 5)) {
					error("Cannot create closure with more than 5 arguments", e.epos);
				} else {
					[];
				};
				var tmp = ident(p, "@tmp");
				(new Tuple(EBlock(::((new Tuple(EVars(::((new Tuple("@tmp", Some(gen_expr(ctx, e2)))), ::((new Tuple("@fun", Some(field(p,
				tmp, f.cf_name)))), []))), p)), ::(if (ctx.macros) {
				call(p, builtin(p, "closure"), ::(ident(p, "@fun"), ::(tmp, [])));
				} else {
					call(p, ident(p, ^ ("@closure", string_of_int(n))), ::(tmp, ::(ident(p, "@fun"), [])));
				}, []))), p));
			case _: assert False;
			};
		case TEnumParameter(e, _, i): (new Tuple(EArray(field(p, gen_expr(ctx, e), "args"), int(p, i)), p));
		case TField(e, f): field(p, gen_expr(ctx, e), field_name(f));
		case TTypeExpr(t): gen_type_path(p, t_path(t));
		case TParenthesis(e): (new Tuple(EParenthesis(gen_expr(ctx, e)), p));
		case TMeta(_, e): gen_expr(ctx, e);
		case TObjectDecl(fl): var hasToString = ref(False);
			var fl = List.map(function (f, e):
			if ( = (f, "toString")) {
			hasToString.val = switch (follow(e.etype)) {
				case TFun([], _): True;
				case _: False;
				};
			} else {
				[];
			};
			(new Tuple(f, gen_expr(ctx, e))), fl);
			(new Tuple(EObject(if (hasToString.val) {
			::((new Tuple("__string", ident(p, "@default__string"))), fl);
			} else {
				fl;
			}), p));
		case TArrayDecl(el): call(p, field(p, ident(p, "Array"), "new1"), ::(array(p, List.map(gen_expr(ctx), el)), ::(int(p,
									  List.length(el)), [])));
		case TCall(e, el): gen_call(ctx, p, e, el);
		case TNew(c, _, params): call(p, field(p, gen_type_path(p, c.cl_path), "new"), List.map(gen_expr(ctx), params));
		case TUnop(op, flag, e): gen_unop(ctx, p, op, flag, e);
		case TVar(v, eo): (new Tuple(EVars(var e = switch (eo) {
		case None: if (v.v_capture) {
					Some(call(p, builtin(p, "array"), ::(null(p), [])));
				} else {
					None;
				};
			case Some(e): var e = gen_expr(ctx, e);
				if (v.v_capture) {
					Some(call(p, builtin(p, "array"), ::(e, [])));
				} else {
					Some(e);
				};
			};
			::((new Tuple(v.v_name, e)), [])), p));
		case TFunction(f): var inits = List.fold_left(function acc: function (a, c): var acc = if (a.v_capture) {
			::((new Tuple(EBinop("=", ident(p, a.v_name), call(p, builtin(p, "array"), ::(ident(p, a.v_name), []))), p)), acc);
			} else {
				acc;
			};
			switch (c) {
		case None | Some(TNull): acc;
			case Some(c): ::(gen_expr(ctx, Codegen.set_default(ctx.com, a, c, e.epos)), acc);
			}, [], f.tf_args);
			var e = gen_expr(ctx, f.tf_expr);
			var e = switch (inits) {
			case []: e;
			case _: (new Tuple(EBlock(List.rev(::(e, inits))), p));
			};
			(new Tuple(EFunction(List.map(arg_name, f.tf_args), with_return(e)), p));
		case TBlock(el): (new Tuple(EBlock(List.map(gen_expr(ctx), el)), p));
		case TFor(v, it, e): var it = gen_expr(ctx, it);
			var e = gen_expr(ctx, e);
			var next = call(p, field(p, ident(p, "@tmp"), "next"), []);
			var next = if (v.v_capture) {
				call(p, builtin(p, "array"), ::(next, []));
			} else {
				next;
			};
			(new Tuple(EBlock(::((new Tuple(EVars(::((new Tuple("@tmp", Some(it))), [])), p)), ::((new Tuple(EWhile(call(p, field(p,
								 ident(p, "@tmp"), "hasNext"), []), (new Tuple(EBlock(::((new Tuple(EVars(::((new Tuple(v.v_name, Some(next))), [])), p)),
										 ::(e, []))), p)), NormalWhile), p)), []))), p));
		case TIf(cond, e1, e2): 	function parent(e) return {
				mk(TParenthesis(e), e.etype, e.epos);
			};
			var e1 = switch (e1.eexpr) {
			case TConst(TInt(n)) if (<(n, )): parent(e1);
			case TConst(TFloat(f)) if (=(f0, '-')): parent(e1);
			case _: e1;
			};
			(new Tuple(EIf(gen_expr(ctx, cond), gen_expr(ctx, e1), switch (e2) {
		case None: None;
		case Some(e): Some(gen_expr(ctx, e));
			}), p));
		case TWhile(econd, e, flag): (new Tuple(EWhile(gen_expr(ctx, econd), gen_expr(ctx, e), switch (flag) {
		case Ast.NormalWhile: NormalWhile;
		case Ast.DoWhile: DoWhile;
		}), p));
		case TTry(e, catchs): function loop(match) return switch (match) {
			case []: call(p, builtin(p, "rethrow"), ::(ident(p, "@tmp"), []));
			case ::((v, e), l): var e2 = loop(l);
				var path = switch (follow(v.v_type)) {
				case TInst(c, _): Some(c.cl_path);
				case TEnum(e, _): Some(e.e_path);
				case TAbstract(a, _): Some(a.a_path);
				case TDynamic(_): None;
				case _: assert False;
				};
				var cond = switch (path) {
				case None: (new Tuple(EConst( True), p));
				case Some(path): call(p, field(p, gen_type_path(p, (new Tuple(::("neko", []), "Boot"))), "__instanceof"), ::(ident(p,
										  "@tmp"), ::(gen_type_path(p, path), [])));
				};
				var id = ident(p, "@tmp");
				var id = if (v.v_capture) {
					call(p, builtin(p, "array"), ::(id, []));
				} else {
					id;
				};
				var e = gen_expr(ctx, e);
				(new Tuple(EIf(cond, (new Tuple(EBlock(::((new Tuple(EVars(::((new Tuple(v.v_name, Some(id))), [])), p)), ::(e, []))), p)),
							   Some(e2)), p));
			};
			var catchs = loop(catchs);
			var catchs = (new Tuple(EBlock(::((new Tuple(EIf((new Tuple(EBinop("==", call(p, builtin(p, "typeof"), ::(ident(p,
											   "@tmp"), [])), builtin(p, "tstring")), p)), (new Tuple(EBinop("=", ident(p, "@tmp"), call(p, field(p, ident(p, "String"),
													   "new"), ::(ident(p, "@tmp"), []))), p)), None), p)), ::(catchs, []))), p));
			(new Tuple(ETry(gen_expr(ctx, e), "@tmp", catchs), p));
		case TReturn(eo): (new Tuple(EReturn(switch (eo) {
		case None: Some(null(p));
			case Some(e): Some(gen_expr(ctx, e));
			}), p));
		case TBreak: (new Tuple(EBreak(None), p));
		case TContinue: (new Tuple(EContinue, p));
		case TThrow(e): call(p, builtin(p, "throw"), ::(gen_expr(ctx, e), []));
		case TCast(e, None): gen_expr(ctx, e);
		case TCast(e1, Some(t)): gen_expr(ctx, Codegen.default_cast(vtmp = "@tmp", ctx.com, e1, t, e.etype, e.epos));
		case TSwitch(e, cases, eo): var e = gen_expr(ctx, e);
			var eo = switch (eo) {
			case None: None;
			case Some(e): Some(gen_expr(ctx, e));
			};
			try {
				(new Tuple(ESwitch(e, List.map(function (el, e2):
				switch (List.map(gen_expr(ctx), el)) {
			case []: assert False;
				case ::(e, []): (new Tuple(e, gen_expr(ctx, e2)));
				case _: raise(Exit);
				}, cases), eo), p));
			} catch (e: Exit) {
				(new Tuple(EBlock(::((new Tuple(EVars(::((new Tuple("@tmp", Some(e))), [])), p)),
				::(List.fold_left(function acc: function (el, e): var cond = switch (el) {
			case []: assert False;
				case ::(e, l): 	function eq(e) return {
						(new Tuple(EBinop("==", ident(p, "@tmp"), gen_expr(ctx, e)), p));
					};
					List.fold_left(function acc: function e: (new Tuple(EBinop("||", acc, eq(e)), p)), eq(e), l);
				};
				(new Tuple(EIf(cond, gen_expr(ctx, e), Some(acc)), p)), switch (eo) {
			case None: null(p);
				case Some(e): e;
				}, List.rev(cases)), []))), p));
			};
		};
	};

	public static function gen_method(ctx, p, c, acc) return {
		ctx.curmethod = c.cf_name;
		if (is_extern_field(c)) {
			acc;
		} else {
			switch (c.cf_expr) {
			case None: ::((new Tuple(c.cf_name, null(p))), acc);
			case Some(e): switch (e.eexpr) {
				case TCall({ eexpr = TField(_, FStatic({ cl_path = (::(neko, []), Lib) }, { cf_name = load | loadLazy = load })) }, ::({ eexpr = TConst(TString(m)) }, ::({ eexpr = TConst(TString(f)) }, ::({ eexpr = TConst(TInt(n)) }, []))))
					: var p = pos(ctx, e.epos);
					var e = call(p, (new Tuple(EField(builtin(p, "loader"), "loadprim"), p)), ::((new Tuple(EBinop("+", (new Tuple(EBinop("+",
								 str(p, m), str(p, "@")), p)), str(p, f)), p)), ::((new Tuple(EConst(Int(Int32.to_int(n))), p)), [])));
					var e = if ( = (load, "load")) {
						e;
					} else {
						(new Tuple(ETry(e, "@e", call(p, ident(p, "@lazy_error"), ::(ident(p, "@e"), []))), p));
					};
					::((new Tuple(c.cf_name, e)), acc);
				case TFunction(_): ::((new Tuple(if (=(c.cf_name, "new")) {
					"__construct__";
				} else {
					c.cf_name;
				}, gen_expr(ctx, e))), acc);
				case _: ::((new Tuple(c.cf_name, null(p))), acc);
				};
			};
		};
	};

	public static function gen_class(ctx, c) return {
		ctx.curclass = s_type_path(c.cl_path);
		ctx.curmethod = "$init";
		var p = pos(ctx, c.cl_pos);
		var clpath = gen_type_path(p, (new Tuple(fst(c.cl_path), ^ ("@", snd(c.cl_path)))));
		var stpath = gen_type_path(p, c.cl_path);
		var fnew = switch (c.cl_constructor) {
		case Some(f): switch (f.cf_expr) {
			case Some({ eexpr = TFunction(tf) }): var params = List.map(function (v, _): v.v_name, tf.tf_args);
				gen_method(ctx, p, f, ::((new Tuple("new", (new Tuple(EFunction(params,
													(new Tuple(EBlock(::((new Tuple(EVars(::((new Tuple("@o", Some(call(p, builtin(p, "new"), ::(null(p), []))))), [])), p)),
															::(call(p, builtin(p, "objsetproto"), ::(ident(p, "@o"), ::(clpath, []))), ::(call(p, builtin(p, "call"), ::(field(p,
																	this(p), "__construct__"), ::(ident(p, "@o"), ::(array(p, List.map(ident(p), params)), [])))),
																	::((new Tuple(EReturn(Some(ident(p, "@o"))), p)), []))))), p))), p)))), []));
			case _: [];
			};
		case None: [];
		};
		var fstring = try {
			var f = PMap.find("toString", c.cl_fields);
			switch (follow(f.cf_type)) {
			case TFun([], _): ::((new Tuple("__string", ident(p, "@default__string"))), []);
			case _: [];
			};
		} catch (e: Not_found) {
			[];
		};
		var fserialize = (new Tuple("__serialize", ident(p, "@serialize")));
		var others = @(switch (c.cl_implements) {
	case []: [];
		case l: ::((new Tuple("__interfaces__", array(p, List.map(function (c, _): gen_type_path(p, c.cl_path), l)))), []);
		}, switch (c.cl_super) {
	case None: [];
		case Some(c, _): ::((new Tuple("__super__", gen_type_path(p, c.cl_path))), []);
		});
		function build(Tuple(f, e)) return {
			(new Tuple(EBinop("=", field(p, ident(p, "@tmp"), f), e), p));
		};
		var tmp = (new Tuple(EVars(::((new Tuple("@tmp", Some(call(p, builtin(p, "new"), ::(null(p), []))))), [])), p));
		var estat = (new Tuple(EBinop("=", stpath, ident(p, "@tmp")), p));
		function gen_props(props) return {
			(new Tuple(EObject(List.map(function (n, s): (new Tuple(n, str(p, s))), props)), p));
		};
		var sprops = switch (Codegen.get_properties(c.cl_ordered_statics)) {
		case []: [];
		case l: ::((new Tuple("__properties__", gen_props(l))), []);
		};
		var sfields = List.map(build, @(::((new Tuple("prototype", clpath)), sprops), PMap.fold(gen_method(ctx, p), c.cl_statics, @(fnew, others))));
		var eclass = (new Tuple(EBinop("=", clpath, ident(p, "@tmp")), p));
		var mfields = List.map(build, PMap.fold(gen_method(ctx, p), c.cl_fields, ::(fserialize, fstring)));
		var props = Codegen.get_properties(c.cl_ordered_fields);
		var emeta = @(::((new Tuple(EBinop("=", field(p, clpath, "__class__"), stpath), p)), switch (props) {
	case []: [];
		case _: var props = gen_props(props);
			var props = switch (c.cl_super) {
			case Some(csup, _) if (Codegen.has_properties(csup)): (new Tuple(EBlock(::((new Tuple(EVars(::((new Tuple("@tmp",
						Some(props))), [])), p)), ::(call(p, builtin(p, "objsetproto"), ::(ident(p, "@tmp"), ::(field(p, field(p, gen_type_path(p,
													 csup.cl_path), "prototype"), "__properties__"), []))), ::(ident(p, "@tmp"), [])))), p));
			case _: props;
			};
			::((new Tuple(EBinop("=", field(p, clpath, "__properties__"), props), p)), []);
		}), switch (c.cl_path) {
	case ([], name): ::((new Tuple(EBinop("=", field(p, ident(p, "@classes"), name), ident(p, name)), p)), []);
		case _: [];
		});
		var emeta = if (ctx.macros) {
			::((new Tuple(EBinop("=", field(p, stpath, "__ct__"), call(p, builtin(p, "typewrap"), ::(Obj.magic(TClassDecl(c)), []))),
						  p)), emeta);
		} else {
			emeta;
		};
		var eextends = switch (c.cl_super) {
		case None: [];
		case Some(c, _): var esuper = gen_type_path(p, (new Tuple(fst(c.cl_path), ^ ("@", snd(c.cl_path)))));
			::(call(p, builtin(p, "objsetproto"), ::(clpath, ::(esuper, []))), []);
		};
		(new Tuple(EBlock(@(::(tmp, ::(eclass, mfields)), @(::(tmp, ::(estat, sfields)), @(eextends, emeta)))), p));
	};

	public static function gen_enum_constr(ctx, path, c) return {
		ctx.curmethod = c.ef_name;
		var p = pos(ctx, c.ef_pos);
		(new Tuple(EBinop("=", field(p, path, c.ef_name), switch (follow(c.ef_type)) {
	case TFun(params, _): var params = List.map(function (n, _, _): n, params);
			(new Tuple(EFunction(params, (new Tuple(EBlock(::((new Tuple(EVars(::((new Tuple("@tmp", Some(EObject(::((new Tuple("tag",
			str(p, c.ef_name))), ::((new Tuple("index", int(p, c.ef_index))), ::((new Tuple("args", array(p, List.map(ident(p),
			params)))), [])))), p))), [])), p)), ::(call(p, builtin(p, "objsetproto"), ::(ident(p, "@tmp"), ::(field(p, path,
			"prototype"), []))), ::(ident(p, "@tmp"), [])))), p))), p));
		case _: (new Tuple(EBlock(::((new Tuple(EVars(::((new Tuple("@tmp", Some(EObject(::((new Tuple("tag", str(p, c.ef_name))),
			::((new Tuple("index", int(p, c.ef_index))), ::((new Tuple("__serialize", ident(p, "@tag_serialize"))), [])))), p))), [])),
			p)), ::(call(p, builtin(p, "objsetproto"), ::(ident(p, "@tmp"), ::(field(p, path, "prototype"), []))), ::(ident(p,
			"@tmp"), [])))), p));
		}), p));
	};

	public static function gen_enum(ctx, e) return {
		ctx.curclass = s_type_path(e.e_path);
		ctx.curmethod = "$init";
		var p = pos(ctx, e.e_pos);
		var path = gen_type_path(p, e.e_path);
		var uname = (new Tuple(EConst(Ident(gen_global_name(ctx, e.e_path))), p));
		(new Tuple(EBlock(@(::((new Tuple(EBinop("=", uname, call(p, builtin(p, "new"), ::(null(p), []))), p)), ::((new Tuple(EBinop("=", path, uname), p)), ::((new Tuple(EBinop("=", field(p, uname, "prototype"), (new Tuple(EObject(::((new Tuple("__enum__", uname)), ::((new Tuple("__serialize", ident(p, "@serialize"))), ::((new Tuple("__string", ident(p, "@enum_to_string"))), [])))), p))), p)), pmap_list(gen_enum_constr(ctx, uname), e.e_constrs)))), switch (e.e_path) {
	case ([], name): ::((new Tuple(EBinop("=", field(p, ident(p, "@classes"), name), ident(p, name)), p)), []);
		case _: [];
		})), p));
	};

	public static function gen_type(ctx, t, acc) return {
		switch (t) {
		case TClassDecl(c): switch (c.cl_init) {
			case None: [];
			case Some(e): ctx.inits = ::((new Tuple(c, e)), ctx.inits);
			};
			if (c.cl_extern) {
				acc;
			} else {
				::(gen_class(ctx, c), acc);
			};
		case TEnumDecl(e): if (e.e_extern) {
				acc;
			} else {
				::(gen_enum(ctx, e), acc);
			};
		case TTypeDecl(_) | TAbstractDecl(_): acc;
		};
	};

	public static function gen_static_vars(ctx, t) return {
		switch (t) {
		case TEnumDecl(_) | TTypeDecl(_) | TAbstractDecl(_): [];
		case TClassDecl(c): if (c.cl_extern) {
				[];
			} else {
				List.fold_right(function f: function acc:
				switch (f.cf_expr) {
			case None: acc;
			case Some(e): switch (e.eexpr) {
					case TFunction(_): acc;
					case _: ctx.curclass = s_type_path(c.cl_path);
						ctx.curmethod = "$statics";
						var p = pos(ctx, e.epos);
						::((new Tuple(EBinop("=", field(p, gen_type_path(p, c.cl_path), f.cf_name), gen_expr(ctx, e)), p)), acc);
					};
				}, c.cl_ordered_statics, []);
			};
		};
	};

	public static function gen_package(ctx, t) return {
		function loop(acc, p) return {
			switch (p) {
			case []: [];
			case ::(x, l): var path = @(acc, ::(x, []));
				if (!(Hashtbl.mem(ctx.packages, path))) {
					var p = pos(ctx, t_infos(t).mt_pos);
					var e = (new Tuple(EBinop("=", gen_type_path(p, (new Tuple(acc, x))), call(p, builtin(p, "new"), ::(null(p), []))), p));
					Hashtbl.add(ctx.packages, path, []);
					switch (acc) {
					case []: var reg = (new Tuple(EBinop("=", field(p, ident(p, "@classes"), x), ident(p, x)), p));
						::(e, ::(reg, loop(path, l)));
					case _: ::(e, loop(path, l));
					};
				} else {
					loop(path, l);
				};
			};
		};
		loop([], fst(t_path(t)));
	};

	public static function gen_boot(ctx) return {
		(new Tuple(EBlock(::((new Tuple(EBinop("=", field(null_pos, gen_type_path(null_pos, (new Tuple(::("neko", []), "Boot"))), "__classes"), ident(null_pos, "@classes")), null_pos)), ::(call(null_pos, field(null_pos, gen_type_path(null_pos, (new Tuple(::("neko", []), "Boot"))), "__init"), []), []))), null_pos));
	};

	public static function gen_name(ctx, acc, t) return {
		switch (t) {
		case TEnumDecl(e) if (e.e_extern): acc;
		case TEnumDecl(e): var p = pos(ctx, e.e_pos);
			var name = @(fst(e.e_path), ::(snd(e.e_path), []));
			var arr = call(p, field(p, ident(p, "Array"), "new1"), ::(array(p, List.map(function n: gen_constant(ctx, e.e_pos,
			TString(n)), name)), ::(int(p, List.length(name)), [])));
			var path = gen_type_path(p, e.e_path);
			var setname = (new Tuple(EBinop("=", field(p, path, "__ename__"), arr), p));
			var arr = call(p, field(p, ident(p, "Array"), "new1"), ::(array(p, List.map(function n: gen_constant(ctx, e.e_pos,
			TString(n)), e.e_names)), ::(int(p, List.length(e.e_names)), [])));
			var setconstrs = (new Tuple(EBinop("=", field(p, path, "__constructs__"), arr), p));
			var meta = switch (Codegen.build_metadata(ctx.com, TEnumDecl(e))) {
			case None: [];
			case Some(e): ::((new Tuple(EBinop("=", field(p, path, "__meta__"), gen_expr(ctx, e)), p)), []);
			};
			var meta = if (ctx.macros) {
				::((new Tuple(EBinop("=", field(p, path, "__et__"), call(p, builtin(p, "typewrap"), ::(Obj.magic(t), []))), p)), meta);
			} else {
				meta;
			};
			@(::(setname, ::(setconstrs, meta)), acc);
		case TClassDecl(c): if ( || (c.cl_extern, switch (c.cl_kind) {
			case KTypeParameter(_): True;
				case _: False;
				})) {
				acc;
			} else {
				var p = pos(ctx, c.cl_pos);
				var name = @(fst(c.cl_path), ::(snd(c.cl_path), []));
				var arr = call(p, field(p, ident(p, "Array"), "new1"), ::(array(p, List.map(function n: gen_constant(ctx, c.cl_pos,
							   TString(n)), name)), ::(int(p, List.length(name)), [])));
				::((new Tuple(EBinop("=", field(p, gen_type_path(p, c.cl_path), "__name__"), arr), p)), switch (c.cl_implements) {
			case []: acc;
				case l: var interf = field(p, gen_type_path(p, c.cl_path), "__interfaces__");
					::((new Tuple(EBinop("=", interf, call(p, field(p, ident(p, "Array"), "new1"), ::(interf, ::(int(p,
														   List.length(l)), [])))), p)), acc);
				});
			};
		case TTypeDecl(_) | TAbstractDecl(_): acc;
		};
	};

	public static function generate_libs_init(match) return switch (match) {
	case []: [];
	case libs: var p = null_pos;
		var es = ident(p, "@s");
		function loadp(n, nargs) return {
			call(p, field(p, builtin(p, "loader"), "loadprim"), ::(str(p, ^ ("std@", n)), ::(int(p, nargs), [])));
		};
		function op(o, e1, e2) return {
			(new Tuple(EBinop(o, e1, e2), p));
		};
		var boot = ::((new Tuple(EVars(::((new Tuple("@s", Some(call(p, loadp("sys_string", 0), [])))), ::((new Tuple("@env",
										  Some(loadp("get_env", 1)))), ::((new Tuple("@b", Some(EIf(op("==", es, str(p, "Windows")), op("+", call(p, ident(p,
												  "@env"), ::(str(p, "HAXEPATH"), [])), str(p, "\\lib\\")), Some(ETry(op("+", call(p, loadp("file_contents", 1), ::(op("+",
														  call(p, ident(p, "@env"), ::(str(p, "HOME"), [])), str(p, "/.haxelib")), [])), str(p, "/")), "e", (new Tuple(EIf(op("==",
																  es, str(p, "Linux")), (new Tuple(EIf(call(p, loadp("sys_exists", 1), ::(str(p, "/usr/lib/haxe/lib"), [])), str(p,
																		  "/usr/lib/haxe/lib/"), Some(str(p, "/usr/share/haxe/lib/"))), p)), Some(str(p, "/usr/local/lib/haxe/lib/"))), p))), p)),
												  p))), [])))), p)), ::((new Tuple(EIf((new Tuple(ETry(op("==", call(p, loadp("sys_file_type", 1), ::(str(p,
														  ".haxelib"), [])), str(p, "dir")), "e", (new Tuple(EConst( False), p))), p)), op("=", ident(p, "@b"), op("+", call(p,
																  loadp("file_full_path", 1), ::(str(p, ".haxelib"), [])), str(p, "/"))), None), p)), ::((new Tuple(EIf(call(p,
																		  loadp("sys_is64", 0), []), op("=", es, op("+", es, int(p, 64))), None), p)), ::(op("=", es, op("+", es, str(p,
																				  "/"))), []))));
		var lpath = field(p, builtin(p, "loader"), "path");
		@(boot, List.map(function dir: var full_path = || ( = (dir0, '/'), = (dir1, ':'));
						 var dstr = str(p, dir);
		op("=", lpath, call(p, builtin(p, "array"), ::(op("+", if (full_path) {
		dstr;
	} else {
		op("+", ident(p, "@b"), dstr);
		}, ident(p, "@s")), ::(lpath, [])))), libs));
	};

	public static function new_context(com, ver, macros) return {
		{
			() with version = ver;
			com = com;
			globals = Hashtbl.create(0);
			curglobal = 0;
			packages = Hashtbl.create(0);
			macros = macros;
			curclass = "$boot";
			curmethod = "$init";
			inits = [];
			label_count = 0
		};
	};

	public static function header([]) return {
		var p = {
			() with psource = "<header>";
			pline = 1
		};
		function fields(l) return {
			function loop(match) return switch (match) {
			case []: assert False;
			case ::(x, []): ident(p, x);
			case ::(x, l): field(p, loop(l), x);
			};
			loop(List.rev(l));
		};
		function func(pl, e) return {
			(new Tuple(EFunction(pl, (new Tuple(EReturn(Some(e)), p))), p));
		};
		var inits = ::((new Tuple("@classes", call(p, builtin(p, "new"), ::(null(p), [])))), ::((new Tuple("@enum_to_string", func([], call(p, fields(::("neko", ::("Boot", ::("__enum_str", [])))), ::(this(p), []))))), ::((new Tuple("@serialize", func([], call(p, fields(::("neko", ::("Boot", ::("__serialize", [])))), ::(this(p), []))))), ::((new Tuple("@tag_serialize", func([], call(p, fields(::("neko", ::("Boot", ::("__tagserialize", [])))), ::(this(p), []))))), ::((new Tuple("@lazy_error", func(::("e", []), call(p, builtin(p, "varargs"), ::(func(::("_", []), call(p, builtin(p, "throw"), ::(ident(p, "e"), []))), []))))), ::((new Tuple("@default__string", func([], (new Tuple(EBlock(::((new Tuple(EVars(::((new Tuple("@s", Some(call(p, field(p, this(p), "toString"), [])))), [])), p)), ::((new Tuple(EIf((new Tuple(EBinop("!=", call(p, builtin(p, "typeof"), ::(ident(p, "@s"), [])), builtin(p, "tobject")), p)), (new Tuple(EReturn(Some(null(p))), p)), None), p)), ::((new Tuple(EReturn(Some(field(p, ident(p, "@s"), "__s"))), p)), [])))), p))))), []))))));
		var inits = @(inits, List.map(function nargs: var args = Array.to_list(Array.init(nargs, function i: Printf.sprintf("%c", char_of_int(+(int_of_char('a'), i)))));
									  var efun = (new Tuple(EFunction(args, (new Tuple(EBlock(::((new Tuple(EBinop("=", (new Tuple(EConst(This), p)), ident(p, "@this")), p)), ::(call(p, ident(p, "@fun"), List.map(ident(p), args)), []))), p))), p));
									  var eif = EIf((new Tuple(EBinop("==", ident(p, "@fun"), null(p)), p)), null(p), Some(efun));
									  var e = func(::("@this", ::("@fun", [])), (new Tuple(eif, p)));
									  (new Tuple( ^ ("@closure", string_of_int(nargs)), e)), ::(0, ::(1, ::(2, ::(3, ::(4, ::(5, []))))))));
		List.map(function (v, e): (new Tuple(EBinop("=", ident(p, v), e), p)), inits);
	};

	public static function build(ctx, types) return {
		var packs = List.concat(List.map(gen_package(ctx), types));
		var names = List.fold_left(gen_name(ctx), [], types);
		var methods = List.rev(List.fold_left(function acc: function t: gen_type(ctx, t, acc), [], types));
		var boot = gen_boot(ctx);
		var inits = List.map(function (c, e): ctx.curclass = s_type_path(c.cl_path);
		ctx.curmethod = "__init__";
		gen_expr(ctx, e), List.rev(ctx.inits));
		ctx.inits = [];
		var vars = List.concat(List.map(gen_static_vars(ctx), types));
		@(packs, @(methods, @(::(boot, names), @(inits, vars))));
	};

	public static function generate(com) return {
		var ctx = new_context(com, if (Common.defined(com, Define.NekoV1)) {
		1;
	} else {
		2;
	}, False);
		var libs = (new Tuple(EBlock(generate_libs_init(com.neko_libs)), {
			() with psource = "<header>";
			pline = 1
		}));
		var el = build(ctx, com.types);
		var emain = switch (com.main) {
		case None: [];
		case Some(e): ::(gen_expr(ctx, e), []);
		};
		var e = (new Tuple(EBlock(@(header([]), @(::(libs, el), emain))), null_pos));
		var source = Common.defined(com, Define.NekoSource);
		var use_nekoc = Common.defined(com, Define.UseNekoc);
		if (!(use_nekoc)) {
			try {
				mkdir_from_path(com.file);
				var ch = IO.output_channel(open_out_bin(com.file));
				Nbytecode.write(ch, Ncompile.compile(ctx.version, e));
				IO.close_out(ch);
			} catch (e: Ncompile.Error(msg)(pos)) {
				var pfile = Common.find_file(com, pos.psource);
				function loop(p) return {
					var pp = {
						() with pfile = pfile;
						pmin = p;
						pmax = p
					};
					if ( >= (Lexer.get_error_line(pp), pos.pline)) {
						pp;
					} else {
						loop(+(p, 1));
					};
				};
				error(msg, loop(0));
			};
		} else {
			[];
		};
		function command(cmd) return {
			try {
				com.run_command(cmd);
			} catch (e: _) {
				-1;
			};
		};
		var neko_file = ^ (try {
			Filename.chop_extension(com.file);
		} catch (e: _) {
			com.file;
		}, ".neko");
		if ( || (source, use_nekoc)) {
			var ch = IO.output_channel(open_out_bin(neko_file));
			Binast.write(ch, e);
			IO.close_out(ch);
		} else {
			[];
		};
		if ( && (use_nekoc, <>(command( ^ ("nekoc", ^ (if ( > (ctx.version, 1)) {
			^ (" -version ", string_of_int(ctx.version));
			} else {
				"";
			}, ^ (" \"", ^ (neko_file, "\""))))), 0))) {
			failwith("Neko compilation failure");
		} else {
			[];
		};
		if (source) {
			if (<>(command( ^ ("nekoc -p \"", ^ (neko_file, "\""))), 0)) {
				failwith("Failed to print neko code");
			} else {
				[];
			};
			Sys.remove(neko_file);
			Sys.rename( ^ (try {
				Filename.chop_extension(com.file);
			} catch (e: _) {
				com.file;
			}, "2.neko"), neko_file);
		} else {
			[];
		};
	}
}
;
