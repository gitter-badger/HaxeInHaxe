import Ast;
import Type;
import Common;

typedef Pos = Ast.Pos;

typedef Sourcemap = {
	sources : DynArray<String>,
	sources_hash : Hashtbl<String, Int>,
	mappings : Rbuffer,
	source_last_line : Int,
	source_last_col : Int,
	source_last_file : Int,
	print_comma : Bool,
	output_last_col : Int,
	output_current_col : Int
};

typedef Ctx = {
	com : Common.Context,
	buf : Rbuffer,
	chan : Out_channel,
	packages : Hashtbl<String, Unit>,
	smap : Sourcemap,
	js_modern : Bool,
	js_flatten : Bool,
	current : Tclass,
	statics : List<Tuple<Tclass, String, Texpr>>,
	inits : List<Texpr>,
	tabs : String,
	in_value : Option<Tvar>,
	in_loop : Bool,
	handle_break : Bool,
	id_counter : Int,
	type_accessor : Module_type -> String,
	separator : Bool,
	found_expose : Bool
};

typedef Object_store = {
	os_name : String,
	os_fields : List<Object_store>
};

class Genjs {
	public static function get_exposed(ctx, path, meta) return {
		try {
			var Tuple(_, args, pos) = Meta.get(Meta.Expose, meta);
			switch (args) {
			case ::((EConst(String(s)), _), []): ::(s, []);
			case []: ::(path, []);
			case _: error("Invalid @:expose parameters", pos);
			};
		} catch (e: Not_found) {
			[];
		};
	};

	public static var dot_path = Ast.s_type_path;

	public static function flat_path(Tuple(p, s)) return {
		function escape(str) return {
			String.concat("_$", ExtString.String.nsplit(str, "_"));
		};
		switch (p) {
		case []: escape(s);
		case _: ^ (String.concat("_", List.map(escape, p)), ^ ("_", escape(s)));
		};
	};

	public static function s_path(ctx) return {
		if (ctx.js_flatten) {
			flat_path;
		} else {
			dot_path;
		};
	};

	public static var kwds = var h = Hashtbl.create(0);
	List.iter(function s: Hashtbl.add(h, s, []), ::("abstract", ::("as", ::("boolean", ::("break", ::("byte", ::("case",
			  ::("catch", ::("char", ::("class", ::("continue", ::("const", ::("debugger", ::("default", ::("delete", ::("do",
										::("double", ::("else", ::("enum", ::("export", ::("extends", ::("false", ::("final", ::("finally", ::("float", ::("for",
												::("function", ::("goto", ::("if", ::("implements", ::("import", ::("in", ::("instanceof", ::("int", ::("interface",
														::("is", ::("let", ::("long", ::("namespace", ::("native", ::("new", ::("null", ::("package", ::("private", ::("protected",
																::("public", ::("return", ::("short", ::("static", ::("super", ::("switch", ::("synchronized", ::("this", ::("throw",
																		::("throws", ::("transient", ::("true", ::("try", ::("typeof", ::("use", ::("var", ::("void", ::("volatile", ::("while",
																				::("with", ::("yield", []))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))));
	h;

	public static var kwds2 = var h = Hashtbl.create(0);
	List.iter(function s: Hashtbl.add(h, s, []), ::("Infinity", ::("NaN", ::("decodeURI", ::("decodeURIComponent",
			  ::("encodeURI", ::("encodeURIComponent", ::("escape", ::("eval", ::("isFinite", ::("isNaN", ::("parseFloat", ::("parseInt",
								 ::("undefined", ::("unescape", ::("JSON", ::("Number", ::("Object", ::("console", ::("window",
										 ::("require", [])))))))))))))))))))));
	h;

	public static function valid_js_ident(s) return {
		&& ( > (String.length(s), 0), try {
			for (i in /*to*/0... - (String.length(s), 1)) {
				switch (String.unsafe_get(s, i)) {
				case 'a' .. 'z' | 'A' .. 'Z' | '$' | '_': [];
				case '0' .. '9' if (>(i, 0)): [];
				case _: raise(Exit);
				};
			};
			True;
		} catch (e: Exit) {
			False;
		});
	};

	public static function field(s) return {
		if ( || (Hashtbl.mem(kwds, s), !(valid_js_ident(s)))) {
			^ ("[\"", ^ (s, "\"]"));
		} else {
			^ (".", s);
		};
	};

	public static function ident(s) return {
		if (Hashtbl.mem(kwds, s)) {
			^ ("$", s);
		} else {
			s;
		};
	};

	public static function check_var_declaration(v) return {
		if (Hashtbl.mem(kwds2, v.v_name)) {
			v.v_name = ^ ("$", v.v_name);
		} else {
			[];
		};
	};

	public static function anon_field(s) return {
		if ( || (Hashtbl.mem(kwds, s), !(valid_js_ident(s)))) {
			^ ("'", ^ (s, "'"));
		} else {
			s;
		};
	};

	public static function static_field(c, s) return {
		switch (s) {
		case length | name if (||(!(c.cl_extern), Meta.has(Meta.HxGen, c.cl_meta))): ^ (".$", s);
		case s: field(s);
		};
	};

	public static function has_feature(ctx) return {
		Common.has_feature(ctx.com);
	};

	public static function add_feature(ctx) return {
		Common.add_feature(ctx.com);
	};

	public static function handle_newlines(ctx, str) return {
		if (ctx.com.debug) {
			function loop(from) return {
				try {
					var next = +(String.index_from(str, from, '\n'), 1);
					Rbuffer.add_char(ctx.smap.mappings, ';');
					ctx.smap.output_last_col = 0;
					ctx.smap.print_comma = False;
					loop(next);
				} catch (e: Not_found) {
					ctx.smap.output_current_col = -(String.length(str), from);
				};
			};
			loop(0);
		} else {
			[];
		};
	};

	public static function flush(ctx) return {
		Rbuffer.output_buffer(ctx.chan, ctx.buf);
		Rbuffer.clear(ctx.buf);
	};

	public static function spr(ctx, s) return {
		ctx.separator = False;
		handle_newlines(ctx, s);
		Rbuffer.add_string(ctx.buf, s);
	};

	public static function print(ctx) return {
		ctx.separator = False;
		Printf.kprintf(function s: handle_newlines(ctx, s);
		Rbuffer.add_string(ctx.buf, s));
	};

	public static function unsupported(p) return {
		error("This expression cannot be compiled to Javascript", p);
	};

	public static function add_mapping(ctx, e) return {
		if ( || (!(ctx.com.debug), < (e.epos.pmin, 0))) {
			[];
		} else {
			var pos = e.epos;
			var smap = ctx.smap;
			var file = try {
				Hashtbl.find(smap.sources_hash, pos.pfile);
			} catch (e: Not_found) {
				var length = DynArray.length(smap.sources);
				Hashtbl.replace(smap.sources_hash, pos.pfile, length);
				DynArray.add(smap.sources, pos.pfile);
				length;
			};
			var Tuple(line, col) = Lexer.find_pos(pos);
			var line = -(line, 1);
			var col = -(col, 1);
			if ( || ( != (smap.source_last_file, file), || ( != (smap.source_last_line, line), != (smap.source_last_col, col)))) {
				if (smap.print_comma) {
					Rbuffer.add_char(smap.mappings, ',');
				} else {
					smap.print_comma = True;
				};
				function base64_vlq(number) return {
					function encode_digit(digit) return {
						var chars = ['A';
						'B';
						'C';
						'D';
						'E';
						'F';
						'G';
						'H';
						'I';
						'J';
						'K';
						'L';
						'M';
						'N';
						'O';
						'P';
						'Q';
						'R';
						'S';
						'T';
						'U';
						'V';
						'W';
						'X';
						'Y';
						'Z';
						'a';
						'b';
						'c';
						'd';
						'e';
						'f';
						'g';
						'h';
						'i';
						'j';
						'k';
						'l';
						'm';
						'n';
						'o';
						'p';
						'q';
						'r';
						's';
						't';
						'u';
						'v';
						'w';
						'x';
						'y';
						'z';
						'0';
						'1';
						'2';
						'3';
						'4';
						'5';
						'6';
						'7';
						'8';
						'9';
						'+';
						'/'];
						Array.unsafe_get(chars, digit);
					};
					function to_vlq(number) return {
						if ( < (number, 0)) {
							+(lsl(~ -(number), 1), 1);
						} else {
							lsl(number, 1);
						};
					};
					function loop(vlq) return {
						var shift = 5;
						var base = lsl(1, shift);
						var mask = -(base, 1);
						var continuation_bit = base;
						var digit = land(vlq, mask);
						var next = asr(vlq, shift);
						Rbuffer.add_char(smap.mappings, encode_digit(if ( > (next, 0)) {
						lor(digit, continuation_bit);
						} else {
							digit;
						}));
						if ( > (next, 0)) {
							loop(next);
						} else {
							[];
						};
					};
					loop(to_vlq(number));
				};
				base64_vlq(-(smap.output_current_col, smap.output_last_col));
				base64_vlq(-(file, smap.source_last_file));
				base64_vlq(-(line, smap.source_last_line));
				base64_vlq(-(col, smap.source_last_col));
				smap.source_last_file = file;
				smap.source_last_line = line;
				smap.source_last_col = col;
				smap.output_last_col = smap.output_current_col;
			} else {
				[];
			};
		};
	};

	public static function write_mappings(ctx) return {
		var basefile = Filename.basename(ctx.com.file);
		print(ctx, "\n//# sourceMappingURL=%s.map", basefile);
		var channel = open_out_bin( ^ (ctx.com.file, ".map"));
		var sources = DynArray.to_list(ctx.smap.sources);
		function to_url(file) return {
			ExtString.String.map(function c:
			if ( == (c, '\\')) {
			'/';
		} else {
			c;
		}, Common.get_full_path(file));
		};
		output_string(channel, "{\n");
		output_string(channel, "\"version\":3,\n");
		output_string(channel, ^ ("\"file\":\"", ^ (String.concat("\\\\", ExtString.String.nsplit(basefile, "\\")), "\",\n")));
		output_string(channel, "\"sourceRoot\":\"file:///\",\n");
		output_string(channel, ^ ("\"sources\":[", ^ (String.concat(",", List.map(function s: ^ ("\"", ^ (to_url(s), "\"")), sources)), "],\n")));
		if (Common.defined(ctx.com, Define.SourceMapContent)) {
			output_string(channel, ^ ("\"sourcesContent\":[", ^ (String.concat(",", List.map(function s:
			try {
				^ ("\"", ^ (Ast.s_escape(Std.input_file(bin = True, s)), "\""));
			} catch (e: _) {
				"null";
			}, sources)), "],\n")));
		} else {
			[];
		};
		output_string(channel, "\"names\":[],\n");
		output_string(channel, "\"mappings\":\"");
		Rbuffer.output_buffer(channel, ctx.smap.mappings);
		output_string(channel, "\"\n");
		output_string(channel, "}");
		close_out(channel);
	};

	public static function newline(ctx) return {
		switch (Rbuffer.nth(ctx.buf, -(Rbuffer.length(ctx.buf), 1))) {
		case '}' | '{' | ':' | ';' if (!(ctx.separator)): print(ctx, "\n%s", ctx.tabs);
		case _: print(ctx, ";\n%s", ctx.tabs);
		};
	};

	public static function newprop(ctx) return {
		switch (Rbuffer.nth(ctx.buf, -(Rbuffer.length(ctx.buf), 1))) {
		case '{': print(ctx, "\n%s", ctx.tabs);
		case _: print(ctx, "\n%s,", ctx.tabs);
		};
	};

	public static function semicolon(ctx) return {
		switch (Rbuffer.nth(ctx.buf, -(Rbuffer.length(ctx.buf), 1))) {
		case '}' if (!(ctx.separator)): [];
		case _: spr(ctx, ";");
		};
	};

	public static function concat(ctx, s, f) return {
	case []: [];
	case ::(x, []): f(x);
	case ::(x, l): f(x);
		spr(ctx, s);
		concat(ctx, s, f, l);
	};

	public static function fun_block(ctx, f, p) return {
		var e = List.fold_left(function e: function (a, c):
		switch (c) {
	case None | Some(TNull): e;
		case Some(c): Type.concat(Codegen.set_default(ctx.com, a, c, p), e);
		}, f.tf_expr, f.tf_args);
		e;
	};

	public static function open_block(ctx) return {
		var oldt = ctx.tabs;
		ctx.tabs = ^ ("\t", ctx.tabs);
		function []: ctx.tabs = oldt;
	};

	public static function has_return(e) return {
		switch (e.eexpr) {
		case TBlock([]): False;
		case TBlock(el): has_return(List.hd(List.rev(el)));
		case TReturn(_): True;
		case _: False;
		};
	};

	public static function iter_switch_break(in_switch, e) return {
		switch (e.eexpr) {
		case TFunction(_) | TWhile(_) | TFor(_): [];
		case TSwitch(_) if (!(in_switch)): iter_switch_break(True, e);
		case TBreak if (in_switch): raise(Exit);
		case _: iter(iter_switch_break(in_switch), e);
		};
	};

	public static function handle_break(ctx, e) return {
		var old = (new Tuple(ctx.in_loop, ctx.handle_break));
		ctx.in_loop = True;
		try {
			iter_switch_break(False, e);
			ctx.handle_break = False;
			function []: ctx.in_loop = fst(old);
			ctx.handle_break = snd(old);
		} catch (e: Exit) {
			spr(ctx, "try {");
			var b = open_block(ctx);
			newline(ctx);
			ctx.handle_break = True;
			function []: b([]);
			ctx.in_loop = fst(old);
			ctx.handle_break = snd(old);
			newline(ctx);
			spr(ctx, "} catch[ e ] { if[ e != \"__break__\" ] throw e; }");
		};
	};

	public static function this(ctx) return {
		switch (ctx.in_value) {
		case None: "this";
		case Some(_): "$this";
		};
	};

	public static function is_dynamic_iterator(ctx, e) return {
		function check(x) return {
			&& (has_feature(ctx, "HxOverrides.iter"), switch (follow(x.etype)) {
		case TInst({ cl_path = ([], Array) }, _) | TInst({ cl_kind = KTypeParameter(_) }, _) | TAnon(_) | TDynamic(_) | TMono(_):
				True;
			case _: False;
			});
		};
		switch (e.eexpr) {
		case TField(x, f) if (=(field_name(f), "iterator")): check(x);
		case _: False;
		};
	};

	public static function gen_constant(ctx, p) return {
	case TInt(i): print(ctx, "%ld", i);
	case TFloat(s): spr(ctx, s);
	case TString(s): print(ctx, "\"%s\"", Ast.s_escape(s));
	case TBool(b): spr(ctx, if (b) {
		"true";
	} else {
		"false";
	});
	case TNull: spr(ctx, "null");
	case TThis: spr(ctx, this(ctx));
	case TSuper: assert False;
	};

	public static function gen_call(ctx, e, el, in_value) return {
		switch ((new Tuple(e.eexpr, el))) {
		case (TConst(TSuper), params): switch (ctx.current.cl_super) {
			case None: error("Missing api.setCurrentClass", e.epos);
			case Some(c, _): print(ctx, "%s.call[%s", ctx.type_accessor(TClassDecl(c)), this(ctx));
				List.iter(function p: print(ctx, ",");
				gen_value(ctx, p), params);
				spr(ctx, "]");
			};
		case (TField({ eexpr = TConst(TSuper) }, f), params):
			switch (ctx.current.cl_super) {
			case None: error("Missing api.setCurrentClass", e.epos);
			case Some(c, _): var name = field_name(f);
				print(ctx, "%s.prototype%s.call[%s", ctx.type_accessor(TClassDecl(c)), field(name), this(ctx));
				List.iter(function p: print(ctx, ",");
						  gen_value(ctx, p), params);
				spr(ctx, "]");
			};
		case (TCall(x, _), el) if (switch (x.eexpr) {
				case TLocal({ v_name = __js__ }): False;
					case _: True;
					}): spr(ctx, "[");
			gen_value(ctx, e);
			spr(ctx, "]");
			spr(ctx, "[");
			concat(ctx, ",", gen_value(ctx), el);
			spr(ctx, "]");
		case (TLocal({ v_name = __new__ }), ::({ eexpr = TConst(TString(cl)) }, params)): print(ctx, "new %s[", cl);
			concat(ctx, ",", gen_value(ctx), params);
			spr(ctx, "]");
		case (TLocal({ v_name = __new__ }), ::(e, params)): spr(ctx, "new ");
			gen_value(ctx, e);
			spr(ctx, "[");
			concat(ctx, ",", gen_value(ctx), params);
			spr(ctx, "]");
		case (TLocal({ v_name = __js__ }), ::({ eexpr = TConst(TString(this)) }, [])): spr(ctx, this(ctx));
		case (TLocal({ v_name = __js__ }), ::({ eexpr = TConst(TString(code)) }, [])): spr(ctx, String.concat("\n",
					ExtString.String.nsplit(code, "\r\n")));
		case (TLocal({ v_name = __js__ }), ::({ eexpr = TConst(TString(code)); epos = p }, tl)): Codegen.interpolate_code(ctx.com,
					code, tl, spr(ctx), gen_expr(ctx), p);
		case (TLocal({ v_name = __instanceof__ }), ::(o, ::(t, []))): spr(ctx, "[");
			gen_value(ctx, o);
			print(ctx, " instanceof ");
			gen_value(ctx, t);
			spr(ctx, "]");
		case (TLocal({ v_name = __typeof__ }), ::(o, [])): spr(ctx, "typeof[");
			gen_value(ctx, o);
			spr(ctx, "]");
		case (TLocal({ v_name = __strict_eq__ }), ::(x, ::(y, []))): spr(ctx, "[[");
			gen_value(ctx, x);
			spr(ctx, "] === ");
			gen_value(ctx, y);
			spr(ctx, "]");
		case (TLocal({ v_name = __strict_neq__ }), ::(x, ::(y, []))): spr(ctx, "[[");
			gen_value(ctx, x);
			spr(ctx, "] !== ");
			gen_value(ctx, y);
			spr(ctx, "]");
		case (TLocal({ v_name = __define_feature__ }), ::(_, ::(e, []))): gen_expr(ctx, e);
		case (TLocal({ v_name = __feature__ }), ::({ eexpr = TConst(TString(f)) }, ::(eif, eelse))):
			if (has_feature(ctx, f)) {
				gen_value(ctx, eif);
			} else {
				switch (eelse) {
				case []: [];
				case ::(e, _): gen_value(ctx, e);
				};
			};
		case (TLocal({ v_name = __rethrow__ }), []): spr(ctx, "throw $hx_rethrow");
		case (TLocal({ v_name = __resources__ }), []): spr(ctx, "[");
			concat(ctx, ",", function (name, data): spr(ctx, "{ ");
				   spr(ctx, "name : ");
				   gen_constant(ctx, e.epos, TString(name));
				   spr(ctx, ", data : ");
				   gen_constant(ctx, e.epos, TString(Codegen.bytes_serialize(data)));
				   spr(ctx, "}"), Hashtbl.fold(function name: function data: function acc: ::((new Tuple(name, data)), acc),
											   ctx.com.resources, []));
			spr(ctx, "]");
		case (TLocal({ v_name = `trace }), ::(e, ::(infos, []))):
			if (has_feature(ctx, "haxe.Log.trace")) {
				var t = try {
					List.find(function t: = (t_path(t), (new Tuple(::("haxe", []), "Log"))), ctx.com.types);
				} catch (e: _) {
					assert False;
				};
				spr(ctx, ctx.type_accessor(t));
				spr(ctx, ".trace[");
				gen_value(ctx, e);
				spr(ctx, ",");
				gen_value(ctx, infos);
				spr(ctx, "]");
			} else {
				spr(ctx, "console.log[");
				gen_value(ctx, e);
				spr(ctx, "]");
			};
		case _: gen_value(ctx, e);
			spr(ctx, "[");
			concat(ctx, ",", gen_value(ctx), el);
			spr(ctx, "]");
		};
	};

	public static function gen_expr(ctx, e) return {
		add_mapping(ctx, e);
		switch (e.eexpr) {
		case TConst(c): gen_constant(ctx, e.epos, c);
		case TLocal(v): spr(ctx, ident(v.v_name));
		case TArray(e1, { eexpr = TConst(TString(s)) }) if (&&(valid_js_ident(s), switch (e1.eexpr) {
			case TConst(TInt(_) | TFloat(_)): False;
				case _: True;
				})): gen_value(ctx, e1);
			spr(ctx, field(s));
		case TArray(e1, e2): gen_value(ctx, e1);
			spr(ctx, "[");
			gen_value(ctx, e2);
			spr(ctx, "]");
		case TBinop(op, { eexpr = TField(x, f) }, e2) if (=(field_name(f), "iterator")): gen_value(ctx, x);
			spr(ctx, field("iterator"));
			print(ctx, " %s ", Ast.s_binop(op));
			gen_value(ctx, e2);
		case TBinop(op, e1, e2): gen_value(ctx, e1);
			print(ctx, " %s ", Ast.s_binop(op));
			gen_value(ctx, e2);
		case TField(x, f) if (&&(=(field_name(f), "iterator"), is_dynamic_iterator(ctx, e))): add_feature(ctx, "use.$iterator");
			print(ctx, "$iterator[");
			gen_value(ctx, x);
			print(ctx, "]");
		case TField(x, FClosure(Some({ cl_path = ([], Array) }, _), { cf_name = push })): add_feature(ctx,
					"use.$arrayPushClosure");
			print(ctx, "$arrayPushClosure[");
			gen_value(ctx, x);
			print(ctx, "]");
		case TField(x, FClosure(_, f)): add_feature(ctx, "use.$bind");
			switch (x.eexpr) {
			case TConst(_) | TLocal(_): print(ctx, "$bind[");
				gen_value(ctx, x);
				print(ctx, ",");
				gen_value(ctx, x);
				print(ctx, "%s]", field(f.cf_name));
			case _: print(ctx, "[$_=");
				gen_value(ctx, x);
				print(ctx, ",$bind[$_,$_%s]]", field(f.cf_name));
			};
		case TEnumParameter(x, _, i): gen_value(ctx, x);
			print(ctx, "[%i]", +(i, 2));
		case TField({ eexpr = TConst(TInt(_) | TFloat(_)) } = x, f): gen_expr(ctx, { (e) with eexpr = TField(mk(TParenthesis(x), x.etype, x.epos), f) });
		case TField(x, FInstance(_, _, f) | FStatic(_, f) | FAnon(f)) if (Meta.has(Meta.SelfCall, f.cf_meta)): gen_value(ctx, x);
		case TField(x, f): gen_value(ctx, x);
			var name = field_name(f);
			spr(ctx, switch (f) {
		case FStatic(c, _): static_field(c, name);
			case FEnum(_) | FInstance(_) | FAnon(_) | FDynamic(_) | FClosure(_): field(name);
			});
		case TTypeExpr(t): spr(ctx, ctx.type_accessor(t));
		case TParenthesis(e): spr(ctx, "[");
			gen_value(ctx, e);
			spr(ctx, "]");
		case TMeta(_, e): gen_expr(ctx, e);
		case TReturn(eo): if (<>(ctx.in_value, None)) {
				unsupported(e.epos);
			} else {
				[];
			};
			switch (eo) {
			case None: spr(ctx, "return");
			case Some(e): spr(ctx, "return ");
				gen_value(ctx, e);
			};
		case TBreak: if (!(ctx.in_loop)) {
				unsupported(e.epos);
			} else {
				[];
			};
			if (ctx.handle_break) {
				spr(ctx, "throw \"__break__\"");
			} else {
				spr(ctx, "break");
			};
		case TContinue: if (!(ctx.in_loop)) {
				unsupported(e.epos);
			} else {
				[];
			};
			spr(ctx, "continue");
		case TBlock(el): print(ctx, "{");
			var bend = open_block(ctx);
			List.iter(gen_block_element(ctx), el);
			bend([]);
			newline(ctx);
			print(ctx, "}");
		case TFunction(f): var old = (new Tuple(ctx.in_value, ctx.in_loop));
			ctx.in_value = None;
			ctx.in_loop = False;
			print(ctx, "function[%s] ", String.concat(",", List.map(ident, List.map(arg_name, f.tf_args))));
			gen_expr(ctx, fun_block(ctx, f, e.epos));
			ctx.in_value = fst(old);
			ctx.in_loop = snd(old);
			ctx.separator = True;
		case TCall(e, el): gen_call(ctx, e, el, False);
		case TArrayDecl(el): spr(ctx, "[");
			concat(ctx, ",", gen_value(ctx), el);
			spr(ctx, "]");
		case TThrow(e): spr(ctx, "throw ");
			gen_value(ctx, e);
		case TVar(v, eo): spr(ctx, "var ");
			check_var_declaration(v);
			spr(ctx, ident(v.v_name));
			switch (eo) {
			case None: [];
			case Some(e): spr(ctx, " = ");
				gen_value(ctx, e);
			};
		case TNew({ cl_path = ([], Array) }, _, []): print(ctx, "[]");
		case TNew(c, _, el): switch (c.cl_constructor) {
			case Some(cf) if (Meta.has(Meta.SelfCall, cf.cf_meta)): [];
			case _: print(ctx, "new ");
			};
			print(ctx, "%s[", ctx.type_accessor(TClassDecl(c)));
			concat(ctx, ",", gen_value(ctx), el);
			spr(ctx, "]");
		case TIf(cond, e, eelse): spr(ctx, "if");
			gen_value(ctx, cond);
			spr(ctx, " ");
			gen_expr(ctx, e);
			switch (eelse) {
			case None: [];
			case Some(e2): switch (e.eexpr) {
				case TObjectDecl(_): ctx.separator = False;
				case _: [];
				};
				semicolon(ctx);
				spr(ctx, " else ");
				gen_expr(ctx, e2);
			};
		case TUnop(op, Ast.Prefix, e): spr(ctx, Ast.s_unop(op));
			gen_value(ctx, e);
		case TUnop(op, Ast.Postfix, e): gen_value(ctx, e);
			spr(ctx, Ast.s_unop(op));
		case TWhile(cond, e, Ast.NormalWhile): var handle_break = handle_break(ctx, e);
			spr(ctx, "while");
			gen_value(ctx, cond);
			spr(ctx, " ");
			gen_expr(ctx, e);
			handle_break([]);
		case TWhile(cond, e, Ast.DoWhile): var handle_break = handle_break(ctx, e);
			spr(ctx, "do ");
			gen_expr(ctx, e);
			semicolon(ctx);
			spr(ctx, " while");
			gen_value(ctx, cond);
			handle_break([]);
		case TObjectDecl(fields): spr(ctx, "{ ");
			concat(ctx, ", ", function (f, e):
			switch (e.eexpr) {
		case TMeta((Meta.QuotedField, _, _), e): print(ctx, "'%s' : ", f);
			case _: print(ctx, "%s : ", anon_field(f));
			};
			gen_value(ctx, e), fields);
			spr(ctx, "}");
			ctx.separator = True;
		case TFor(v, it, e): check_var_declaration(v);
			var handle_break = handle_break(ctx, e);
			var it = ident(switch (it.eexpr) {
		case TLocal(v): v.v_name;
			case _: var id = ctx.id_counter;
				ctx.id_counter = +(ctx.id_counter, 1);
				var name = ^ ("$it", string_of_int(id));
				print(ctx, "var %s = ", name);
				gen_value(ctx, it);
				newline(ctx);
				name;
			});
			print(ctx, "while[ %s.hasNext[] ] {", it);
			var bend = open_block(ctx);
			newline(ctx);
			print(ctx, "var %s = %s.next[]", ident(v.v_name), it);
			gen_block_element(ctx, e);
			bend([]);
			newline(ctx);
			spr(ctx, "}");
			handle_break([]);
		case TTry(e, catchs): spr(ctx, "try ");
			gen_expr(ctx, e);
			var vname = switch (catchs) {
			case ::((v, _), []): check_var_declaration(v);
				v.v_name;
			case _: var id = ctx.id_counter;
				ctx.id_counter = +(ctx.id_counter, 1);
				^ ("$e", string_of_int(id));
			};
			print(ctx, " catch[ %s ] {", vname);
			var bend = open_block(ctx);
			var last = ref(False);
			var else_block = ref(False);
			if (has_feature(ctx, "haxe.CallStack.exceptionStack")) {
				newline(ctx);
				print(ctx, "%s.lastException = %s", ctx.type_accessor(TClassDecl({ (null_class) with cl_path = (new Tuple(::("haxe", []), "CallStack")) })),
					  vname);
			} else {
				[];
			};
			if (has_feature(ctx, "js.Lib.rethrow")) {
				function has_rethrow(Tuple(_, e)) return {
					function loop(e) return {
						switch (e.eexpr) {
						case TCall({ eexpr = TLocal({ v_name = __rethrow__ }) }, []): raise(Exit);
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
				if (List.exists(has_rethrow, catchs)) {
					newline(ctx);
					print(ctx, "var $hx_rethrow = %s", vname);
				} else {
					[];
				};
			} else {
				[];
			};
			if (has_feature(ctx, "js.Boot.HaxeError")) {
				newline(ctx);
				print(ctx, "if [%s instanceof %s] %s = %s.val", vname, ctx.type_accessor(TClassDecl({ (null_class) with cl_path = (new Tuple(::("js", ::("_Boot", [])), "HaxeError")) })),
					  vname, vname);
			} else {
				[];
			};
			List.iter(function (v, e):
			if (last.val) {
			[];
			} else {
				var t = switch (follow(v.v_type)) {
				case TEnum(e, _): Some(TEnumDecl(e));
				case TInst(c, _): Some(TClassDecl(c));
				case TAbstract(a, _): Some(TAbstractDecl(a));
				case TFun(_) | TLazy(_) | TType(_) | TAnon(_): assert False;
				case TMono(_) | TDynamic(_): None;
				};
				switch (t) {
				case None: last.val = True;
					if (else_block.val) {
						print(ctx, "{");
					} else {
						[];
					};
					if (<>(vname, v.v_name)) {
						newline(ctx);
						print(ctx, "var %s = %s", v.v_name, vname);
					} else {
						[];
					};
					gen_block_element(ctx, e);
					if (else_block.val) {
						newline(ctx);
						print(ctx, "}");
					} else {
						[];
					};
				case Some(t): if (!(else_block.val)) {
						newline(ctx);
					} else {
						[];
					};
					print(ctx, "if[ %s.__instanceof[%s,", ctx.type_accessor(TClassDecl({ (null_class) with cl_path = (new Tuple(::("js", []), "Boot")) })),
						  vname);
					gen_value(ctx, mk(TTypeExpr(t), mk_mono([]), e.epos));
					spr(ctx, "] ] {");
					var bend = open_block(ctx);
					if (<>(vname, v.v_name)) {
						newline(ctx);
						print(ctx, "var %s = %s", v.v_name, vname);
					} else {
						[];
					};
					gen_block_element(ctx, e);
					bend([]);
					newline(ctx);
					spr(ctx, "} else ");
					else_block.val = True;
				};
			}, catchs);
			if (!(last.val)) {
				print(ctx, "throw[%s]", vname);
			} else {
				[];
			};
			bend([]);
			newline(ctx);
			spr(ctx, "}");
		case TSwitch(e, cases, def): spr(ctx, "switch");
			gen_value(ctx, e);
			spr(ctx, " {");
			newline(ctx);
			List.iter(function (el, e2): List.iter(function e:
			switch (e.eexpr) {
		case TConst(c) if (=(c, TNull)): spr(ctx, "case null: case undefined:");
			case _: spr(ctx, "case ");
				gen_value(ctx, e);
				spr(ctx, ":");
			}, el);
			var bend = open_block(ctx);
					   gen_block_element(ctx, e2);
			if (!(has_return(e2))) {
			newline(ctx);
				print(ctx, "break");
			} else {
				[];
			};
			bend([]);
			newline(ctx), cases);
			switch (def) {
			case None: [];
			case Some(e): spr(ctx, "default:");
				var bend = open_block(ctx);
				gen_block_element(ctx, e);
				bend([]);
				newline(ctx);
			};
			spr(ctx, "}");
		case TCast(e, None): gen_expr(ctx, e);
		case TCast(e1, Some(t)): print(ctx, "%s.__cast[", ctx.type_accessor(TClassDecl({ (null_class) with cl_path = (new Tuple(::("js", []), "Boot")) })));
			gen_expr(ctx, e1);
			spr(ctx, " , ");
			spr(ctx, ctx.type_accessor(t));
			spr(ctx, "]");
		};
	};

	public static function gen_block_element( ? : (after = False), ctx, e) return {
		switch (e.eexpr) {
		case TBlock(el): List.iter(gen_block_element(after = , ctx), el);
		case TCall({ eexpr = TLocal({ v_name = __feature__ }) }, ::({ eexpr = TConst(TString(f)) }, ::(eif, eelse))):
			if (has_feature(ctx, f)) {
				gen_block_element(after = , ctx, eif);
			} else {
				switch (eelse) {
				case []: [];
				case ::(e, []): gen_block_element(after = , ctx, e);
				case _: assert False;
				};
			};
		case TFunction(_): gen_block_element(after = , ctx, mk(TParenthesis(e), e.etype, e.epos));
		case TObjectDecl(fl): List.iter(function (_, e): gen_block_element(after = , ctx, e), fl);
		case _: if (!(after)) {
				newline(ctx);
			} else {
				[];
			};
			gen_expr(ctx, e);
			if (after) {
				newline(ctx);
			} else {
				[];
			};
		};
	};

	public static function gen_value(ctx, e) return {
		add_mapping(ctx, e);
		function assign(e) return {
			mk(TBinop(Ast.OpAssign, mk(TLocal(switch (ctx.in_value) {
		case None: assert False;
		case Some(v): v;
			}), t_dynamic, e.epos), e), e.etype, e.epos);
		};
		function value([]) return {
			var old = (new Tuple(ctx.in_value, ctx.in_loop));
			var r = alloc_var("$r", t_dynamic);
			ctx.in_value = Some(r);
			ctx.in_loop = False;
			spr(ctx, "[function[$this] ");
			spr(ctx, "{");
			var b = open_block(ctx);
			newline(ctx);
			spr(ctx, "var $r");
			newline(ctx);
			function []: newline(ctx);
			spr(ctx, "return $r");
			b([]);
			newline(ctx);
			spr(ctx, "}");
			ctx.in_value = fst(old);
			ctx.in_loop = snd(old);
			print(ctx, "[%s]]", this(ctx));
		};
		switch (e.eexpr) {
		case TConst(_) | TLocal(_) | TArray(_) | TBinop(_) | TField(_) | TEnumParameter(_) | TTypeExpr(_) | TParenthesis(_) | TObjectDecl(_) | TArrayDecl(_) | TNew(_) | TUnop(_) | TFunction(_)
				: gen_expr(ctx, e);
		case TMeta(_, e1): gen_value(ctx, e1);
		case TCall(e, el): gen_call(ctx, e, el, True);
		case TReturn(_) | TBreak | TContinue: unsupported(e.epos);
		case TCast(e1, None): gen_value(ctx, e1);
		case TCast(e1, Some(t)): print(ctx, "%s.__cast[", ctx.type_accessor(TClassDecl({ (null_class) with cl_path = (new Tuple(::("js", []), "Boot")) })));
			gen_value(ctx, e1);
			spr(ctx, " , ");
			spr(ctx, ctx.type_accessor(t));
			spr(ctx, "]");
		case TVar(_) | TFor(_) | TWhile(_) | TThrow(_): var v = value([]);
			gen_expr(ctx, e);
			v([]);
		case TBlock(::(e, [])): gen_value(ctx, e);
		case TBlock(el): var v = value([]);
			function loop(match) return switch (match) {
			case []: spr(ctx, "return null");
			case ::(e, []): gen_expr(ctx, assign(e));
			case ::(e, l): gen_expr(ctx, e);
				newline(ctx);
				loop(l);
			};
			loop(el);
			v([]);
		case TIf(cond, e, eo): var cond = switch (cond.eexpr) {
			case TParenthesis({ eexpr = TBinop(Ast.OpAssign | Ast.OpAssignOp(_), _, _) | TIf(_) }): cond;
			case TParenthesis(e): e;
			case _: cond;
			};
			gen_value(ctx, cond);
			spr(ctx, "?");
			gen_value(ctx, e);
			spr(ctx, ":");
			switch (eo) {
			case None: spr(ctx, "null");
			case Some(e): gen_value(ctx, e);
			};
		case TSwitch(cond, cases, def): var v = value([]);
			gen_expr(ctx, mk(TSwitch(cond, List.map(function (e1, e2): (new Tuple(e1, assign(e2))), cases), switch (def) {
		case None: None;
		case Some(e): Some(assign(e));
			}), e.etype, e.epos));
			v([]);
		case TTry(b, catchs): var v = value([]);
			function block(e) return {
				mk(TBlock(::(e, [])), e.etype, e.epos);
			};
			gen_expr(ctx, mk(TTry(block(assign(b)), List.map(function (v, e): (new Tuple(v, block(assign(e)))), catchs)), e.etype,
							 e.epos));
			v([]);
		};
	};

	public static function generate_package_create(ctx, Tuple(p, _)) return {
		function loop(acc) return {
		case []: [];
		case ::(p, l) if (Hashtbl.mem(ctx.packages, ::(p, acc))): loop(::(p, acc), l);
		case ::(p, l): Hashtbl.add(ctx.packages, ::(p, acc), []);
			switch (acc) {
			case []: if (ctx.js_modern) {
					print(ctx, "var %s = {}", p);
				} else {
					print(ctx, "var %s = %s || {}", p, p);
				};
			case _: var p = ^ (String.concat(".", List.rev(acc)), field(p));
				if (ctx.js_modern) {
					print(ctx, "%s = {}", p);
				} else {
					print(ctx, "if[!%s] %s = {}", p, p);
				};
			};
			ctx.separator = True;
			newline(ctx);
			loop(::(p, acc), l);
		};
		switch (p) {
		case []: print(ctx, "var ");
		case _: loop([], p);
		};
	};

	public static function check_field_name(c, f) return {
		switch (f.cf_name) {
		case prototype | __proto__ | constructor: error( ^ ("The field name '", ^ (f.cf_name, "'  is not allowed in JS")),
			switch (f.cf_expr) {
		case None: c.cl_pos;
		case Some(e): e.epos;
			});
		case _: [];
		};
	};

	public static function path_to_brackets(path) return {
		var parts = ExtString.String.nsplit(path, ".");
		^ ("[\"", ^ (String.concat("\"][\"", parts), "\"]"));
	};

	public static function gen_class_static_field(ctx, c, f) return {
		switch (f.cf_expr) {
		case None | Some({ eexpr = TConst(TNull) }) if (!(has_feature(ctx, "Type.getClassFields"))): [];
		case None if (is_extern_field(f)): [];
		case None: print(ctx, "%s%s = null", s_path(ctx, c.cl_path), static_field(c, f.cf_name));
			newline(ctx);
		case Some(e): switch (e.eexpr) {
			case TFunction(_): var path = ^ (s_path(ctx, c.cl_path), static_field(c, f.cf_name));
				var dot_path = ^ (dot_path(c.cl_path), static_field(c, f.cf_name));
				ctx.id_counter = 0;
				print(ctx, "%s = ", path);
				switch (get_exposed(ctx, dot_path, f.cf_meta)) {
				case ::(s, []): print(ctx, "$hx_exports%s = ", path_to_brackets(s));
				case _: [];
				};
				gen_value(ctx, e);
				newline(ctx);
			case _: ctx.statics = ::((new Tuple(c, f.cf_name, e)), ctx.statics);
			};
		};
	};

	public static function can_gen_class_field(ctx) return {
	case { cf_expr = None | Some({ eexpr = TConst(TNull) }) } if (!(has_feature(ctx, "Type.getInstanceFields"))): False;
	case f: !(is_extern_field(f));
	};

	public static function gen_class_field(ctx, c, f) return {
		check_field_name(c, f);
		switch (f.cf_expr) {
		case None: newprop(ctx);
			print(ctx, "%s: ", anon_field(f.cf_name));
			print(ctx, "null");
		case Some(e): newprop(ctx);
			print(ctx, "%s: ", anon_field(f.cf_name));
			ctx.id_counter = 0;
			gen_value(ctx, e);
			ctx.separator = False;
		};
	};

	public static function generate_class___name__(ctx, c) return {
		if (has_feature(ctx, "js.Boot.isClass")) {
			var p = s_path(ctx, c.cl_path);
			print(ctx, "%s.__name__ = ", p);
			if (has_feature(ctx, "Type.getClassName")) {
				print(ctx, "[%s]", String.concat(",", List.map(function s: Printf.sprintf("\"%s\"", Ast.s_escape(s)), @(fst(c.cl_path),
				::(snd(c.cl_path), [])))));
			} else {
				print(ctx, "true");
			};
			newline(ctx);
		} else {
			[];
		};
	};

	public static function generate_class(ctx, c) return {
		ctx.current = c;
		ctx.id_counter = 0;
		switch (c.cl_path) {
		case ([], Function): error("This class redefine a native one", c.cl_pos);
		case _: [];
		};
		var p = s_path(ctx, c.cl_path);
		var hxClasses = has_feature(ctx, "Type.resolveClass");
		if (ctx.js_flatten) {
			print(ctx, "var ");
		} else {
			generate_package_create(ctx, c.cl_path);
		};
		if ( || (ctx.js_modern, !(hxClasses))) {
			print(ctx, "%s = ", p);
		} else {
			print(ctx, "%s = $hxClasses[\"%s\"] = ", p, dot_path(c.cl_path));
		};
		switch (get_exposed(ctx, dot_path(c.cl_path), c.cl_meta)) {
		case ::(s, []): print(ctx, "$hx_exports%s = ", path_to_brackets(s));
		case _: [];
		};
		switch (c.cl_kind) {
		case KAbstractImpl(_): print(ctx, "{}");
			ctx.separator = True;
		case _: switch (c.cl_constructor) {
			case Some({ cf_expr = Some(e) }): gen_expr(ctx, e);
			case _: print(ctx, "function[] { }");
				ctx.separator = True;
			};
		};
		newline(ctx);
		if ( && (ctx.js_modern, hxClasses)) {
			print(ctx, "$hxClasses[\"%s\"] = %s", dot_path(c.cl_path), p);
			newline(ctx);
		} else {
			[];
		};
		generate_class___name__(ctx, c);
		switch (c.cl_implements) {
		case []: [];
		case l: print(ctx, "%s.__interfaces__ = [%s]", p, String.concat(",", List.map(function (i,
						  _): ctx.type_accessor(TClassDecl(i)), l)));
			newline(ctx);
		};
		function gen_props(props) return {
			String.concat(",", List.map(function (p, v): ^ (p, ^ (":\"", ^ (v, "\""))), props));
		};
		var has_property_reflection = || (has_feature(ctx, "Reflect.getProperty"), has_feature(ctx, "Reflect.setProperty"));
		if (has_property_reflection) {
			switch (Codegen.get_properties(c.cl_ordered_statics)) {
			case []: [];
			case props: print(ctx, "%s.__properties__ = {%s}", p, gen_props(props));
				newline(ctx);
			};
		} else {
			[];
		};
		List.iter(gen_class_static_field(ctx, c), c.cl_ordered_statics);
		var has_class = && (has_feature(ctx, "js.Boot.getClass"), || (<>(c.cl_super, None), || (<>(c.cl_ordered_fields, []), <>(c.cl_constructor, None))));
		var has_prototype = || (<>(c.cl_super, None), || (has_class, List.exists(can_gen_class_field(ctx), c.cl_ordered_fields)));
		if (has_prototype) {
			switch (c.cl_super) {
			case None: print(ctx, "%s.prototype = {", p);
			case Some(csup, _): var psup = ctx.type_accessor(TClassDecl(csup));
				print(ctx, "%s.__super__ = %s", p, psup);
				newline(ctx);
				print(ctx, "%s.prototype = $extend[%s.prototype,{", p, psup);
			};
			var bend = open_block(ctx);
			List.iter(function f:
			if (can_gen_class_field(ctx, f)) {
			gen_class_field(ctx, c, f);
			} else {
				[];
			}, c.cl_ordered_fields);
			if (has_class) {
				newprop(ctx);
				print(ctx, "__class__: %s", p);
			} else {
				[];
			};
			if (has_property_reflection) {
				var props = Codegen.get_properties(c.cl_ordered_fields);
				switch (c.cl_super) {
				case _ if (=(props, [])): [];
				case Some(csup, _) if (Codegen.has_properties(csup)): newprop(ctx);
					var psup = s_path(ctx, csup.cl_path);
					print(ctx, "__properties__: $extend[%s.prototype.__properties__,{%s}]", psup, gen_props(props));
				case _: newprop(ctx);
					print(ctx, "__properties__: {%s}", gen_props(props));
				};
			} else {
				[];
			};
			bend([]);
			print(ctx, "\n}");
			switch (c.cl_super) {
			case None: ctx.separator = True;
			case _: print(ctx, "]");
			};
			newline(ctx);
		} else {
			[];
		};
		flush(ctx);
	};

	public static function generate_enum(ctx, e) return {
		var p = s_path(ctx, e.e_path);
		var ename = List.map(function s: Printf.sprintf("\"%s\"", Ast.s_escape(s)), @(fst(e.e_path), ::(snd(e.e_path), [])));
		if (ctx.js_flatten) {
			print(ctx, "var ");
		} else {
			generate_package_create(ctx, e.e_path);
		};
		print(ctx, "%s = ", p);
		if (has_feature(ctx, "Type.resolveEnum")) {
			print(ctx, "$hxClasses[\"%s\"] = ", dot_path(e.e_path));
		} else {
			[];
		};
		print(ctx, "{");
		if (has_feature(ctx, "js.Boot.isEnum")) {
			print(ctx, " __ename__ : %s,", if (has_feature(ctx, "Type.getEnumName")) {
			^ ("[", ^ (String.concat(",", ename), "]"));
			} else {
				"true";
			});
		} else {
			[];
		};
		print(ctx, " __constructs__ : [%s] }", String.concat(",", List.map(function s: Printf.sprintf("\"%s\"", s), e.e_names)));
		ctx.separator = True;
		newline(ctx);
		List.iter(function n: var f = PMap.find(n, e.e_constrs);
				  print(ctx, "%s%s = ", p, field(f.ef_name));
		switch (f.ef_type) {
	case TFun(args, _): var sargs = String.concat(",", List.map(function (n, _, _): ident(n), args));
			print(ctx, "function[%s] { var $x = [\"%s\",%d,%s]; $x.__enum__ = %s;", sargs, f.ef_name, f.ef_index, sargs, p);
			if (has_feature(ctx, "has_enum")) {
				spr(ctx, " $x.toString = $estr;");
			} else {
				[];
			};
			spr(ctx, " return $x; }");
			ctx.separator = True;
		case _: print(ctx, "[\"%s\",%d]", f.ef_name, f.ef_index);
			newline(ctx);
			if (has_feature(ctx, "has_enum")) {
				print(ctx, "%s%s.toString = $estr", p, field(f.ef_name));
				newline(ctx);
			} else {
				[];
			};
			print(ctx, "%s%s.__enum__ = %s", p, field(f.ef_name), p);
		};
		newline(ctx), e.e_names);
		if (has_feature(ctx, "Type.allEnums")) {
			var ctors_without_args = List.filter(function s: var ef = PMap.find(s, e.e_constrs);
			switch (follow(ef.ef_type)) {
		case TFun(_): False;
			case _: True;
			}, e.e_names);
			print(ctx, "%s.__empty_constructs__ = [%s]", p, String.concat(",", List.map(function s: Printf.sprintf("%s.%s", p, s),
					ctors_without_args)));
			newline(ctx);
		} else {
			[];
		};
		switch (Codegen.build_metadata(ctx.com, TEnumDecl(e))) {
		case None: [];
		case Some(e): print(ctx, "%s.__meta__ = ", p);
			gen_expr(ctx, e);
			newline(ctx);
		};
		flush(ctx);
	};

	public static function generate_static(ctx, Tuple(c, f, e)) return {
		print(ctx, "%s%s = ", s_path(ctx, c.cl_path), static_field(c, f));
		gen_value(ctx, e);
		newline(ctx);
	};

	public static function generate_require(ctx, path, meta) return {
		var Tuple(_, args, mp) = Meta.get(Meta.JsRequire, meta);
		var p = s_path(ctx, path);
		if (ctx.js_flatten) {
			spr(ctx, "var ");
		} else {
			generate_package_create(ctx, path);
		};
		switch (args) {
		case ::((EConst(String(module_name)), _), []): print(ctx, "%s = require[\"%s\"]", p, module_name);
		case ::((EConst(String(module_name)), _), ::((EConst(String(object_path)), _), [])): print(ctx, "%s = require[\"%s\"].%s",
					p, module_name, object_path);
		case _: error("Unsupported @:jsRequire format", mp);
		};
		newline(ctx);
	};

	public static function generate_type(ctx) return {
	case TClassDecl(c): switch (c.cl_init) {
		case None: [];
		case Some(e): ctx.inits = ::(e, ctx.inits);
		};
		var p = s_path(ctx, c.cl_path);
		if ( = (p, "Math")) {
			generate_class___name__(ctx, c);
		} else {
			[];
		};
		if ( && ( = (p, "Std"), = (c.cl_ordered_statics, []))) {
			[];
		} else {
			if (!(c.cl_extern)) {
				generate_class(ctx, c);
			} else {
				if ( && (Meta.has(Meta.JsRequire, c.cl_meta), is_directly_used(ctx.com, c.cl_meta))) {
					generate_require(ctx, c.cl_path, c.cl_meta);
				} else {
					if ( && (!(ctx.js_flatten), Meta.has(Meta.InitPackage, c.cl_meta))) {
						switch (c.cl_path) {
						case ([], _): [];
						case _: generate_package_create(ctx, c.cl_path);
						};
					} else {
						[];
					};
				};
			};
		};
	case TEnumDecl(e) if (e.e_extern):
		if ( && (Meta.has(Meta.JsRequire, e.e_meta), is_directly_used(ctx.com, e.e_meta))) {
			generate_require(ctx, e.e_path, e.e_meta);
		} else {
			[];
		};
	case TEnumDecl(e): generate_enum(ctx, e);
	case TTypeDecl(_) | TAbstractDecl(_): [];
	};

	public static function set_current_class(ctx, c) return {
		ctx.current = c;
	};

	public static function alloc_ctx(com) return {
		var ctx = {
			() with com = com;
			buf = Rbuffer.create(16000);
			chan = open_out_bin(com.file);
			packages = Hashtbl.create(0);
			smap = {
				() with source_last_line = 0;
				source_last_col = 0;
				source_last_file = 0;
				print_comma = False;
				output_last_col = 0;
				output_current_col = 0;
				sources = DynArray.create([]);
				sources_hash = Hashtbl.create(0);
				mappings = Rbuffer.create(16)
			};
			js_modern = !(Common.defined(com, Define.JsClassic));
			js_flatten = !(Common.defined(com, Define.JsUnflatten));
			statics = [];
			inits = [];
			current = null_class;
			tabs = "";
			in_value = None;
			in_loop = False;
			handle_break = False;
			id_counter = 0;
			type_accessor = function _: assert False;
			separator = False;
			found_expose = False
		};
		ctx.type_accessor = function t: var p = t_path(t);
		switch (t) {
		case TClassDecl({ cl_extern = True } = c) if (!(Meta.has(Meta.JsRequire, c.cl_meta))): dot_path(p);
		case TEnumDecl({ e_extern = True } = e) if (!(Meta.has(Meta.JsRequire, e.e_meta))): dot_path(p);
		case _: s_path(ctx, p);
		};
		ctx;
	};

	public static function gen_single_expr(ctx, e, expr) return {
		if (expr) {
			gen_expr(ctx, e);
		} else {
			gen_value(ctx, e);
		};
		var str = Rbuffer.unsafe_contents(ctx.buf);
		Rbuffer.reset(ctx.buf);
		ctx.id_counter = 0;
		str;
	};

	public static function generate(com) return {
		switch (com.js_gen) {
		case Some(g): g([]);
		case None: var ctx = alloc_ctx(com);
			Codegen.map_source_header(com, function s: print(ctx, "// %s\n", s));
			if ( || (has_feature(ctx, "Class"), has_feature(ctx, "Type.getClassName"))) {
				add_feature(ctx, "js.Boot.isClass");
			} else {
				[];
			};
			if ( || (has_feature(ctx, "Enum"), has_feature(ctx, "Type.getEnumName"))) {
				add_feature(ctx, "js.Boot.isEnum");
			} else {
				[];
			};
			var nodejs = Common.raw_defined(com, "nodejs");
			var exposed = List.concat(List.map(function t:
			switch (t) {
		case TClassDecl(c): var path = dot_path(c.cl_path);
				var class_exposed = get_exposed(ctx, path, c.cl_meta);
				var static_exposed = List.map(function f: get_exposed(ctx, ^ (path, static_field(c, f.cf_name)), f.cf_meta),
											  c.cl_ordered_statics);
				List.concat(::(class_exposed, static_exposed));
			case _: [];
			}, com.types));
			var anyExposed = <>(exposed, []);
			var exportMap = ref(PMap.create(String.compare));
			var exposedObject = { () with os_name = "";
								  os_fields = []
								};
			var toplevelExposed = ref([]);
			List.iter(function path: var parts = ExtString.String.nsplit(path, ".");
			function loop(p, pre) return {
				switch (p) {
				case ::(f, ::(g, ls)): var path = switch (pre) {
					case : f;
					case pre: ^ (pre, ^ (".", f));
					};
					if (!(PMap.exists(path, exportMap.val))) {
						var elts = { () with os_name = f;
									 os_fields = []
								   };
						exportMap.val = PMap.add(path, elts, exportMap.val);
						var cobject = switch (pre) {
						case : exposedObject;
						case pre: PMap.find(pre, exportMap.val);
						};
						cobject.os_fields = ::(elts, cobject.os_fields);
					} else {
						[];
					};
					loop(::(g, ls), path);
				case ::(f, []) if (=(pre, "")): toplevelExposed.val = ::(f, toplevelExposed.val);
				case _: [];
				};
			};
			loop(parts, ""), exposed);
			var include_files = List.rev(com.include_files);
			List.iter(function file:
			switch (file) {
		case (path, top): var file_content = Std.input_file(bin = True, fst(file));
				print(ctx, "%s\n", file_content);
				[];
			case _: [];
			}, include_files);
			var var_console = (new Tuple("console", "typeof console != \"undefined\" ? console : {log:function[]{}}"));
			var var_exports = (new Tuple("$hx_exports",
										 "typeof window != \"undefined\" ? window : typeof exports != \"undefined\" ? exports : typeof self != \"undefined\" ? self : this"));
			var var_global = (new Tuple("$global",
										"typeof window != \"undefined\" ? window : typeof global != \"undefined\" ? global : typeof self != \"undefined\" ? self : this"));
			var closureArgs = [];
			var closureArgs = if (has_feature(ctx, "js.Lib.global")) {
				::(var_global, closureArgs);
			} else {
				closureArgs;
			};
			var closureArgs = if ( && (anyExposed, !(Common.defined(com, Define.ShallowExpose)))) {
				::(var_exports, closureArgs);
			} else {
				closureArgs;
			};
			var closureArgs = if (!(Common.defined(com, Define.JsEs5))) {
				::(var_console, closureArgs);
			} else {
				closureArgs;
			};
			if (nodejs) {
				List.iter(function s: Hashtbl.replace(kwds2, s, []), ::("global", ::("process", ::("__filename", ::("__dirname",
						  ::("module", []))))));
			} else {
				[];
			};
			if ( && (anyExposed, || (Common.defined(com, Define.ShallowExpose), !(ctx.js_modern)))) {
				print(ctx, "var %s = %s", fst(var_exports), snd(var_exports));
				ctx.separator = True;
				newline(ctx);
			} else {
				[];
			};
			if (ctx.js_modern) {
				List.iter(function s: Hashtbl.replace(kwds, s, []), ::("arguments", ::("eval", [])));
				print(ctx, "[function [%s] { \"use strict\"", String.concat(", ", List.map(fst, closureArgs)));
				newline(ctx);
			} else {
				[];
			};
			function print_obj(f, root) return {
				var path = ^ (root, path_to_brackets(f.os_name));
				print(ctx, "%s = %s || {}", path, path);
				ctx.separator = True;
				newline(ctx);
				concat(ctx, ";", function g: print_obj(g, path), f.os_fields);
			};
			List.iter(function f: print_obj(f, "$hx_exports"), exposedObject.os_fields);
			List.iter(function file:
			switch (file) {
		case (path, closure): var file_content = Std.input_file(bin = True, fst(file));
				print(ctx, "%s\n", file_content);
				[];
			case _: [];
			}, include_files);
			if ( && (!(ctx.js_modern), !(Common.defined(com, Define.JsEs5)))) {
				add_feature(ctx, "js.Lib.global");
			} else {
				[];
			};
			if ( && (!(ctx.js_modern), has_feature(ctx, "js.Lib.global"))) {
				print(ctx, "var %s = %s;\n", fst(var_global), snd(var_global));
			} else {
				[];
			};
			if ( && (!(ctx.js_modern), !(Common.defined(com, Define.JsEs5)))) {
				spr(ctx, "var console = $global.console || {log:function[]{}};\n");
			} else {
				[];
			};
			var vars = [];
			var vars = if ( || (has_feature(ctx, "Type.resolveClass"), has_feature(ctx, "Type.resolveEnum"))) {
				::( ^ ("$hxClasses = ", if (ctx.js_modern) {
				"{}";
			} else {
				"$hxClasses || {}";
			}), vars);
			} else {
				vars;
			};
			var vars = if (has_feature(ctx, "has_enum")) {
				::( ^ ("$estr = function[] { return ", ^ (ctx.type_accessor(TClassDecl({ (null_class) with cl_path = (new Tuple(::("js", []), "Boot")) })),
						".__string_rec[this,'']; }")), vars);
			} else {
				vars;
			};
			switch (List.rev(vars)) {
			case []: [];
			case vl: print(ctx, "var %s", String.concat(",", vl));
				ctx.separator = True;
				newline(ctx);
			};
		if (List.exists(function case TClassDecl({ cl_extern = False; cl_super = Some(_) }): True;
							case _: False, com.types)) {
						print(ctx, "function $extend[from, fields] {
							  function Inherit[] {} Inherit.prototype = from; var proto = new Inherit[];
							  for [var name in fields] proto[name] = fields[name];
							  if[ fields.toString !== Object.prototype.toString ] proto.toString = fields.toString;
							  return proto;
						  }
							  ");
					} else {
				[];
			};
			List.iter(generate_type(ctx), com.types);
			function chk_features(e) return {
				if (is_dynamic_iterator(ctx, e)) {
					add_feature(ctx, "use.$iterator");
				} else {
					[];
				};
				switch (e.eexpr) {
				case TField(_, FClosure(_)): add_feature(ctx, "use.$bind");
				case _: Type.iter(chk_features, e);
				};
			};
			List.iter(chk_features, ctx.inits);
			List.iter(function (_, _, e): chk_features(e), ctx.statics);
			if (has_feature(ctx, "use.$iterator")) {
				add_feature(ctx, "use.$bind");
				print(ctx,
					  "function $iterator[o] { if[ o instanceof Array ] return function[] { return HxOverrides.iter[o]; }; return typeof[o.iterator] == 'function' ? $bind[o,o.iterator] : o.iterator; }");
				newline(ctx);
			} else {
				[];
			};
			if (has_feature(ctx, "use.$bind")) {
				print(ctx, "var $_, $fid = 0");
				newline(ctx);
				print(ctx,
					  "function $bind[o,m] { if[ m == null ] return null; if[ m.__id__ == null ] m.__id__ = $fid++; var f; if[ o.hx__closures__ == null ] o.hx__closures__ = {}; else f = o.hx__closures__[m.__id__]; if[ f == null ] { f = function[]{ return f.method.apply[f.scope, arguments]; }; f.scope = o; f.method = m; o.hx__closures__[m.__id__] = f; } return f; }");
				newline(ctx);
			} else {
				[];
			};
			if (has_feature(ctx, "use.$arrayPushClosure")) {
				print(ctx, "function $arrayPushClosure[a] {");
				print(ctx, " return function[x] { a.push[x]; }; ");
				print(ctx, "}");
				newline(ctx);
			} else {
				[];
			};
			List.iter(gen_block_element(after = True, ctx), List.rev(ctx.inits));
			List.iter(generate_static(ctx), List.rev(ctx.statics));
			switch (com.main) {
			case None: [];
			case Some(e): gen_expr(ctx, e);
				newline(ctx);
			};
			if (ctx.js_modern) {
				print(ctx, "}][%s]", String.concat(", ", List.map(snd, closureArgs)));
				newline(ctx);
			} else {
				[];
			};
			if ( && (anyExposed, Common.defined(com, Define.ShallowExpose))) {
				List.iter(function f: print(ctx, "var %s = $hx_exports%s", f.os_name, path_to_brackets(f.os_name));
						  ctx.separator = True;
						  newline(ctx), exposedObject.os_fields);
				List.iter(function f: print(ctx, "var %s = $hx_exports%s", f, path_to_brackets(f));
						  ctx.separator = True;
						  newline(ctx), toplevelExposed.val);
			} else {
				[];
			};
			if (com.debug) {
				write_mappings(ctx);
			} else {
				try {
					Sys.remove( ^ (com.file, ".map"));
				} catch (e: _) {
					[];
				};
			};
			flush(ctx);
			close_out(ctx.chan);
		};
	}
}
;
