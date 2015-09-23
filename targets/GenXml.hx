import Ast;
import Type;
import Common;
import ExtString;

enum Xml {
	Node(value: StringList<Tuple<String, String>>List<Xml>);
	PCData(value: String);
	CData(value: String);
};

class Genxml {
	public static function tag(name) return {
		Node(name, [], []);
	};

	public static function xml(name, att) return {
		Node(name, att, []);
	};

	public static function node(name, att, childs) return {
		Node(name, att, childs);
	};

	public static function pcdata(s) return {
		PCData(s);
	};

	public static function cdata(s) return {
		CData(s);
	};

	public static function pmap(f, m) return {
		PMap.fold(function x: function acc: ::(f(x), acc), m, []);
	};

	public static function gen_path(Tuple(p, n), priv) return {
		(new Tuple("path", String.concat(".", @(p, ::(n, [])))));
	};

	public static function gen_string(s) return {
		if ( || (String.contains(s, '<'), || (String.contains(s, '>'), String.contains(s, '&')))) {
			cdata(s);
		} else {
			pcdata(s);
		};
	};

	public static function gen_doc(s) return {
		var s = ExtString.String.strip(s);
		var s = String.concat("\n", ExtString.String.nsplit(String.concat("\n", ExtString.String.nsplit(s, "\r\n")), "\r"));
		node("haxe_doc", [], ::(gen_string(s), []));
	};

	public static function gen_doc_opt(d) return {
		switch (d) {
		case None: [];
		case Some(s): ::(gen_doc(s), []);
		};
	};

	public static function gen_arg_name(Tuple(name, opt, _)) return {
		^ (if (opt) {
		"?";
	} else {
		"";
	}, name);
	};

	public static function real_path(path, meta) return {
		function loop(match) return switch (match) {
		case []: path;
		case ::((Meta.RealPath, ::((Ast.EConst(Ast.String(s)), _), []), _), _): parse_path(s);
		case ::(_, l): loop(l);
		};
		loop(meta);
	};

	public static function tpath(t) return {
		var i = t_infos(t);
		real_path(i.mt_path, i.mt_meta);
	};

	public static function follow_param(t) return {
		switch (t) {
		case TMono(r): switch (r.val) {
			case Some(t): follow_param(t);
			case _: t;
			};
		case TType({ t_path = ([], Null) } = t, tl): follow_param(apply_params(t.t_params, tl, t.t_type));
		case _: t;
		};
	};

	public static function gen_meta(meta) return {
		var meta = List.filter(function (m, _, _):
		switch (m) {
	case Meta.Used | Meta.MaybeUsed | Meta.RealPath: False;
	case _: True;
	}, meta);
		switch (meta) {
		case []: [];
		case _: var nodes = List.map(function (m, el, _): node("m", ::((new Tuple("n", fst(MetaInfo.to_string(m)))), []),
			List.map(function e: node("e", [], ::(gen_string(Ast.s_expr(e)), [])), el)), meta);
			::(node("meta", [], nodes), []);
		};
	};

	public static function gen_type( ? : (values = None), t) return {
		switch (t) {
		case TMono(m): switch (m.val) {
			case None: tag("unknown");
			case Some(t): gen_type(t);
			};
		case TEnum(e, params): gen_type_decl("e", TEnumDecl(e), params);
		case TInst(c, params): gen_type_decl("c", TClassDecl(c), params);
		case TAbstract(a, params): gen_type_decl("x", TAbstractDecl(a), params);
		case TType(t, params): gen_type_decl("t", TTypeDecl(t), params);
		case TFun(args, r): var names = String.concat(":", List.map(gen_arg_name, args));
			var values = switch (values) {
			case None: [];
			case Some(values): var has_value = ref(False);
				var values = List.map(function (n, _, _):
				try {
					var e = PMap.find(n, values);
					has_value.val = True;
					var s = Ast.s_expr(e);
					s;
				} catch (e: Not_found) {
					"";
				}, args);
				if (has_value.val) {
					::((new Tuple("v", String.concat(":", values))), []);
				} else {
					[];
				};
			};
			var args = List.map(function (_, opt, t):
			if (opt) {
			follow_param(t);
			} else {
				t;
			}, args);
			node("f", ::((new Tuple("a", names)), values), List.map(gen_type, @(args, ::(r, []))));
		case TAnon(a): node("a", [], pmap(function f: gen_field([], { (f) with cf_public = False }), a.a_fields));
		case TDynamic(t2): node("d", [], if ( == (t, t2)) {
			[];
			} else {
				::(gen_type(t2), []);
			});
		case TLazy(f): gen_type(f.val([]));
		};
	};

	public static function gen_type_decl(n, t, pl) return {
		var i = t_infos(t);
		node(n, ::(gen_path(tpath(t), i.mt_private), []), List.map(gen_type, pl));
	};

	public static function gen_field(att, f) return {
		function add_get_set(acc, name, att) return {
			switch (acc) {
			case AccNormal | AccResolve | AccRequire(_): att;
			case AccNo | AccNever: ::((new Tuple(name, "null")), att);
			case AccCall: ::((new Tuple(name, "accessor")), att);
			case AccInline: ::((new Tuple(name, "inline")), att);
			};
		};
		var att = switch (f.cf_expr) {
		case None: att;
		case Some(e): ::((new Tuple("line", string_of_int(Lexer.get_error_line(e.epos)))), att);
		};
		var Tuple(att, values) = switch (f.cf_kind) {
		case Var(v): var att = try {
				switch (Meta.get(Meta.Value, f.cf_meta)) {
				case (_, ::(e, []), _): ::((new Tuple("expr", Ast.s_expr(e))), att);
				case _: att;
				};
			} catch (e: Not_found) {
				att;
			};
			(new Tuple(add_get_set(v.v_read, "get", add_get_set(v.v_write, "set", att)), PMap.empty));
		case Method(m): var att = switch (m) {
			case MethNormal | MethMacro: ::((new Tuple("set", "method")), att);
			case MethDynamic: ::((new Tuple("set", "dynamic")), att);
			case MethInline: ::((new Tuple("get", "inline")), ::((new Tuple("set", "null")), att));
			};
			(new Tuple(att, get_value_meta(f.cf_meta)));
		};
		var att = switch (f.cf_params) {
		case []: att;
		case l: ::((new Tuple("params", String.concat(":", List.map(function (n, _): n, l)))), att);
		};
		var overloads = switch (List.map(gen_field([]), f.cf_overloads)) {
		case []: [];
		case nl: ::(node("overloads", [], nl), []);
		};
		function field_name(cf) return {
			try {
				switch (Meta.get(Meta.RealPath, cf.cf_meta)) {
				case (_, ::((EConst(String(s)), _), []), _): s;
				case _: raise(Not_found);
				};
			} catch (e: Not_found) {
				cf.cf_name;
			};
		};
		node(field_name(f), if (f.cf_public) {
		::((new Tuple("public", "1")), att);
		} else {
			att;
		}, @(::(gen_type(values = Some(values), f.cf_type), gen_meta(f.cf_meta)), @(gen_doc_opt(f.cf_doc), overloads)));
	};

	public static function gen_constr(e) return {
		var doc = gen_doc_opt(e.ef_doc);
		var Tuple(args, t) = switch (follow(e.ef_type)) {
		case TFun(args, _): (new Tuple(::((new Tuple("a", String.concat(":", List.map(gen_arg_name, args)))), []),
			@(List.map(function (_, opt, t): gen_type(if (opt) {
			follow_param(t);
			} else {
				t;
			}), args), doc)));
		case _: (new Tuple([], doc));
		};
		node(e.ef_name, args, @(t, gen_meta(e.ef_meta)));
	};

	public static function gen_ordered_constr(e) return {
		function loop(el) return {
			switch (el) {
			case ::(n, el): ::(gen_constr(PMap.find(n, e.e_constrs)), loop(el));
			case []: [];
			};
		};
		loop(e.e_names);
	};

	public static function gen_type_params(ipos, priv, path, params, pos, m) return {
		var mpriv = if (priv) {
			::((new Tuple("private", "1")), []);
		} else {
			[];
		};
		var mpath = if (<>(m.m_path, path)) {
			::((new Tuple("module", snd(gen_path(m.m_path, False)))), []);
		} else {
			[];
		};
		var file = if ( && (ipos, <>(pos, null_pos))) {
			::((new Tuple("file", pos.pfile)), []);
		} else {
			[];
		};
		::(gen_path(path, priv), ::((new Tuple("params", String.concat(":", List.map(fst, params)))), @(file, @(mpriv, mpath))));
	};

	public static function gen_class_path(name, Tuple(c, pl)) return {
		node(name, ::((new Tuple("path", s_type_path(tpath(TClassDecl(c))))), []), List.map(gen_type, pl));
	};

	public static function exists(f, c) return {
		|| (PMap.exists(f.cf_name, c.cl_fields), switch (c.cl_super) {
	case None: False;
	case Some(csup, _): exists(f, csup);
		});
	};

	public static function gen_type_decl(com, pos, t) return {
		var m = t_infos(t).mt_module;
		switch (t) {
		case TClassDecl(c): var stats = List.filter(function cf:
			&& (<>(cf.cf_name, "__meta__"), !(Meta.has(Meta.GenericInstance, cf.cf_meta))), c.cl_ordered_statics);
			var stats = List.map(gen_field(::((new Tuple("static", "1")), [])), stats);
			var fields = List.filter(function cf: !(Meta.has(Meta.GenericInstance, cf.cf_meta)), c.cl_ordered_fields);
			var fields = switch (c.cl_super) {
			case None: List.map(function f: (new Tuple(f, [])), fields);
			case Some(csup, _): List.map(function f: if (exists(f, csup)) {
				(new Tuple(f, ::((new Tuple("override", "1")), [])));
				} else {
					(new Tuple(f, []));
				}, fields);
			};
			var fields = List.map(function (f, att): gen_field(att, f), fields);
			var constr = switch (c.cl_constructor) {
			case None: [];
			case Some(f): ::(gen_field([], f), []);
			};
			var impl = List.map(gen_class_path(if (c.cl_interface) {
			"extends";
		} else {
			"implements";
		}), c.cl_implements);
			var tree = switch (c.cl_super) {
			case None: impl;
			case Some(x): ::(gen_class_path("extends", x), impl);
			};
			var doc = gen_doc_opt(c.cl_doc);
			var meta = gen_meta(c.cl_meta);
			var ext = if (c.cl_extern) {
				::((new Tuple("extern", "1")), []);
			} else {
				[];
			};
			var interf = if (c.cl_interface) {
				::((new Tuple("interface", "1")), []);
			} else {
				[];
			};
			var dynamic = switch (c.cl_dynamic) {
			case None: [];
			case Some(t): ::(node("haxe_dynamic", [], ::(gen_type(t), [])), []);
			};
			node("class", @(gen_type_params(pos, c.cl_private, tpath(t), c.cl_params, c.cl_pos, m), @(ext, interf)), @(tree, @(stats,
					@(fields, @(constr, @(doc, @(meta, dynamic)))))));
		case TEnumDecl(e): var doc = gen_doc_opt(e.e_doc);
			var meta = gen_meta(e.e_meta);
			node("enum", gen_type_params(pos, e.e_private, tpath(t), e.e_params, e.e_pos, m), @(gen_ordered_constr(e), @(doc, meta)));
		case TTypeDecl(t): var doc = gen_doc_opt(t.t_doc);
			var meta = gen_meta(t.t_meta);
			var tt = gen_type(t.t_type);
			node("typedef", gen_type_params(pos, t.t_private, t.t_path, t.t_params, t.t_pos, m), @(::(tt, doc), meta));
		case TAbstractDecl(a): var doc = gen_doc_opt(a.a_doc);
			var meta = gen_meta(a.a_meta);
			function mk_cast(t) return {
				node("icast", [], ::(gen_type(t), []));
			};
			function mk_field_cast(Tuple(t, cf)) return {
				node("icast", ::((new Tuple("field", cf.cf_name)), []), ::(gen_type(t), []));
			};
			var sub = switch ((new Tuple(a.a_from, a.a_from_field))) {
			case ([], []): [];
			case (l1, l2): ::(node("from", [], @(List.map(mk_cast, l1), List.map(mk_field_cast, l2))), []);
			};
			var super = switch ((new Tuple(a.a_to, a.a_to_field))) {
			case ([], []): [];
			case (l1, l2): ::(node("to", [], @(List.map(mk_cast, l1), List.map(mk_field_cast, l2))), []);
			};
			var impl = switch (a.a_impl) {
			case None: [];
			case Some(c): ::(node("impl", [], ::(gen_type_decl(com, pos, TClassDecl(c)), [])), []);
			};
			var this = ::(node("this", [], ::(gen_type(a.a_this), [])), []);
			node("abstract", gen_type_params(pos, a.a_private, tpath(t), a.a_params, a.a_pos, m), @(sub, @(this, @(super, @(doc,
					@(meta, impl))))));
		};
	};

	public static function escape_entities(s) return {
		Xml.to_string(Xml.PCData(s));
	};

	public static function att_str(att) return {
		String.concat("", List.map(function (a, v): Printf.sprintf(" %s=\"%s\"", a, escape_entities(v)), att));
	};

	public static function write_xml(ch, tabs, x) return {
		switch (x) {
		case Node(name, att, []): IO.printf(ch, "%s<%s%s/>", tabs, name, att_str(att));
		case Node(name, att, ::(x, [])): IO.printf(ch, "%s<%s%s>", tabs, name, att_str(att));
			write_xml(ch, "", x);
			IO.printf(ch, "</%s>", name);
		case Node(name, att, childs): IO.printf(ch, "%s<%s%s>\n", tabs, name, att_str(att));
			List.iter(function x: write_xml(ch, ^ (tabs, "\t"), x);
			IO.printf(ch, "\n"), childs);
			IO.printf(ch, "%s</%s>", tabs, name);
		case PCData(s): IO.printf(ch, "%s", s);
		case CData(s): IO.printf(ch, "<![CDATA[%s]]>", s);
		};
	};

	public static function generate(com, file) return {
		var t = Common.timer("construct xml");
		var x = node("haxe", [], List.map(gen_type_decl(com, True), List.filter(function t: !(Meta.has(Meta.NoDoc, t_infos(t).mt_meta)), com.types)));
		t([]);
		var t = Common.timer("write xml");
		var ch = IO.output_channel(open_out_bin(file));
		write_xml(ch, "", x);
		IO.close_out(ch);
		t([]);
	};

	public static function gen_type_string(ctx, t) return {
		var x = gen_type_decl(ctx, False, t);
		var ch = IO.output_string([]);
		write_xml(ch, "", x);
		IO.close_out(ch);
	};

	public static function create_dir(acc) return {
	case []: [];
	case ::(d, l): var path = ^ (acc, ^ ("/", d));
		try {
			Unix.mkdir(path, 0o777);
		} catch (e: _) {
			[];
		};
		create_dir(path, l);
	};

	public static function conv_path(p) return {
		switch (List.rev(fst(p))) {
		case ::(x, l) if (=(x0, '_')): (new Tuple(List.rev(::( ^ ("priv", x), l)), snd(p)));
		case _: p;
		};
	};

	public static function get_real_path(meta, path) return {
		try {
			var real_path = switch (Meta.get(Meta.RealPath, meta)) {
			case (_, ::((EConst(String(s)), _), []), _): s;
			case _: raise(Not_found);
			};
			switch (List.rev(String.nsplit(real_path, "."))) {
			case ::(name, pack): (new Tuple(List.rev(pack), name));
			case _: raise(Not_found);
			};
		} catch (e: Not_found) {
			path;
		};
	};

	public static function generate_type(com, t) return {
		var base_path = "hxclasses";
		var Tuple(pack, name) = var info = t_infos(t);
		get_real_path(info.mt_meta, info.mt_path);
		create_dir(".", ::(base_path, pack));
		switch ((new Tuple(pack, name))) {
		case (::(flash, ::(net, [])), NetStreamPlayTransitions) | (::(flash, ::(filters, [])), BitmapFilterQuality) | (::(flash, ::(display, [])), BitmapDataChannel | GraphicsPathCommand)
				: [];
		case _: var f = open_out_bin( ^ (base_path, ^ ("/", ^ (switch (pack) {
		case []: "";
			case l: ^ (String.concat("/", l), "/");
			}, ^ (name, ".hx")))));
			var ch = IO.output_channel(f);
			function p(fmt) return {
				IO.printf(ch, fmt);
			};
			if (<>(pack, [])) {
				IO.printf(ch, "package %s;\n\n", String.concat(".", pack));
			} else {
				[];
			};
			function notnull(t) return {
				switch (t) {
				case TMono(r): switch (r.val) {
					case None: t;
					case Some(t): notnull(t);
					};
				case TLazy(f): notnull(f.val([]));
				case TType({ t_path = ([], Null) }, ::(t, [])): t;
				case _: t;
				};
			};
			function path(meta, p, tl) return {
				var p = conv_path(get_real_path(meta, p));
				^ (if ( = (fst(p), pack)) {
				snd(p);
				} else {
					s_type_path(p);
				}, switch (tl) {
			case []: "";
				case _: ^ ("<", ^ (String.concat(",", List.map(stype, tl)), ">"));
				});
			};
			function stype(t) return {
				switch (t) {
				case TMono(r): switch (r.val) {
					case None: "Unknown";
					case Some(t): stype(t);
					};
				case TInst({ cl_kind = KTypeParameter(_) } = c, tl): path([], (new Tuple([], snd(c.cl_path))), tl);
				case TInst(c, tl): path(c.cl_meta, c.cl_path, tl);
				case TEnum(e, tl): path(e.e_meta, e.e_path, tl);
				case TType(t, tl): path(t.t_meta, t.t_path, tl);
				case TAbstract(a, tl): path(a.a_meta, a.a_path, tl);
				case TAnon(a): var fields = PMap.fold(function f: function acc: ::( ^ (f.cf_name, ^ (" : ", stype(f.cf_type))), acc),
														  a.a_fields, []);
					^ ("{", ^ (String.concat(", ", fields), "}"));
				case TLazy(f): stype(f.val([]));
				case TDynamic(t2): if ( == (t, t2)) {
						"Dynamic";
					} else {
						^ ("Dynamic<", ^ (stype(t2), ">"));
					};
				case TFun([], ret): ^ ("Void -> ", ftype(ret));
				case TFun(args, ret): ^ (String.concat(" -> ", List.map(function (_, _, t): ftype(t), args)), ^ (" -> ", ftype(ret)));
				};
			};
			function ftype(t) return {
				switch (t) {
				case TMono(r): switch (r.val) {
					case None: stype(t);
					case Some(t): ftype(t);
					};
				case TLazy(f): ftype(f.val([]));
				case TFun(_): ^ ("[", ^ (stype(t), "]"));
				case _: stype(t);
				};
			};
			function sparam(Tuple(n, v, t)) return {
				switch (v) {
				case None: ^ (n, ^ (" : ", stype(t)));
				case Some(Ident(null)): if (is_nullable(notnull(t))) {
						^ ("?", ^ (n, ^ (" : ", stype(notnull(t)))));
					} else {
						^ (n, ^ (" : ", ^ (stype(t), ^ (" = ", switch (follow(t)) {
					case TAbstract({ a_path = ([], Int | Float | UInt) }, _): "0";
						case TAbstract({ a_path = ([], Bool) }, _): "false";
						case _: "null";
						}))));
					};
				case Some(v): ^ (n, ^ (" : ", ^ (stype(t), ^ (" = ", switch (s_constant(v)) {
				case nan: "0./*NaN*/";
				case v: v;
				}))));
				};
			};
			function print_meta(ml) return {
				List.iter(function (m, pl, _):
				switch (m) {
			case Meta.DefParam | Meta.CoreApi | Meta.Used | Meta.MaybeUsed | Meta.FlatEnum | Meta.Value | Meta.DirectlyUsed: [];
				case _: switch (pl) {
					case []: p("@%s ", fst(MetaInfo.to_string(m)));
					case l: p("@%s[%s] ", fst(MetaInfo.to_string(m)), String.concat(",", List.map(Ast.s_expr, pl)));
					};
				}, ml);
			};
			function access(is_read, a) return {
				switch ((new Tuple(a, pack))) {
				case (AccNever, ::(flash, _)): "null";
				case _: s_access(is_read, a);
				};
			};
			function print_field(stat, f) return {
				p("\t");
				print_meta(f.cf_meta);
				if (stat) {
					p("static ");
				} else {
					[];
				};
				var name = try {
					switch (Meta.get(Meta.RealPath, f.cf_meta)) {
					case (Meta.RealPath, ::((EConst(String(s)), _), []), _): s;
					case _: raise(Not_found);
					};
				} catch (e: Not_found) {
					f.cf_name;
				};
				switch (f.cf_kind) {
				case Var(v): p("var %s", name);
					if ( || (<>(v.v_read, AccNormal), <>(v.v_write, AccNormal))) {
						p("[%s,%s]", access(True, v.v_read), access(False, v.v_write));
					} else {
						[];
					};
					p(" : %s", stype(f.cf_type));
				case Method(m): var Tuple(params, ret) = switch (follow(f.cf_type)) {
					case TFun(args, ret): (new Tuple(List.map(function (a, o, t): function loop(match) return switch (match) {
					case []: Ident("null");
						case ::((Meta.DefParam, ::((EConst(String(p)), _), ::((EConst(v), _), [])), _), _) if (=(p, a)):
							switch (v) {
							case Float(1.#QNAN): Float("0./*NaN*/");
							case Float(4294967295.): Int("0xFFFFFFFF");
							case Int(16777215): Int("0xFFFFFF");
							case Float(x): try {
									var f = float_of_string(x);
									var s = string_of_int(int_of_float(f));
									if ( = ( ^ (s, "."), x)) {
										Int(s);
									} else {
										v;
									};
								} catch (e: _) {
									v;
								};
							case _: v;
							};
						case ::(_, l): loop(l);
						};
						(new Tuple(a, if (o) {
						Some(loop(f.cf_meta));
						} else {
							None;
						}, t)), args), ret));
					case _: assert False;
					};
					var tparams = switch (f.cf_params) {
					case []: "";
					case l: ^ ("<", ^ (String.concat(",", List.map(fst, l)), ">"));
					};
					p("function %s%s[%s] : %s", name, tparams, String.concat(", ", List.map(sparam, params)), stype(ret));
				};
				p(";\n");
				if (Meta.has(Meta.Overload, f.cf_meta)) {
					List.iter(function f: print_field(stat, f), f.cf_overloads);
				} else {
					[];
				};
			};
			switch (t) {
			case TClassDecl(c): print_meta(c.cl_meta);
				p("extern %s %s", if (c.cl_interface) {
				"interface";
			} else {
				"class";
			}, stype(TInst(c, List.map(snd, c.cl_params))));
				var ext = switch (c.cl_super) {
				case None: [];
				case Some(c, pl): ::(^(" extends ", stype(TInst(c, pl))), []);
				};
				var ext = List.fold_left(function acc: function (i, pl): ::( ^ (if (c.cl_interface) {
				" extends ";
			} else {
				" implements ";
			}, stype(TInst(i, pl))), acc), ext, c.cl_implements);
				var ext = switch (c.cl_dynamic) {
				case None: ext;
				case Some(t): switch (c.cl_path) {
					case (::(flash, ::(errors, [])), _): ext;
					case _ if (==(t, t_dynamic)): ::(" implements Dynamic", ext);
					case _: ::(^(" implements Dynamic<", ^(stype(t), ">")), ext);
					};
				};
				var ext = switch (c.cl_path) {
				case (::(flash, ::(utils, [])), ByteArray): ::(" implements ArrayAccess<Int>", ext);
				case (::(flash, ::(utils, [])), Dictionnary): ::(" implements ArrayAccess<Dynamic>", []);
				case (::(flash, ::(xml, [])), XML): ::(" implements Dynamic<XMLList>", []);
				case (::(flash, ::(xml, [])), XMLList): ::(" implements ArrayAccess<XML>", []);
				case (::(flash, ::(display, [])), MovieClip): ::(" extends Sprite #if !flash_strict implements Dynamic #end", []);
				case (::(flash, ::(errors, [])), Error): ::(" #if !flash_strict implements Dynamic #end", []);
				case _: ext;
				};
				p("%s", String.concat("", List.rev(ext)));
				p(" {\n");
				function sort(l) return {
					var a = Array.of_list(List.filter(function f: && (f.cf_public, !(List.memq(f, c.cl_overrides))), l));
					function name(match) return switch (match) {
					case new: "";
					case n: n;
					};
					Array.sort(function f1: function f2:
					switch ((new Tuple(f1.cf_kind, f2.cf_kind))) {
				case (Var(_), Var(_)) | (Method(_), Method(_)): compare(name(f1.cf_name), name(f2.cf_name));
					case (Var(_), _): -1;
					case _: 1;
					}, a);
					Array.to_list(a);
				};
				List.iter(print_field(False), sort(switch (c.cl_constructor) {
			case None: c.cl_ordered_fields;
			case Some(f): ::(f, c.cl_ordered_fields);
				}));
				List.iter(print_field(True), sort(c.cl_ordered_statics));
				p("}\n");
			case TEnumDecl(e): print_meta(e.e_meta);
				p("extern enum %s {\n", stype(TEnum(e, List.map(snd, e.e_params))));
				function sort(l) return {
					var a = Array.of_list(l);
					Array.sort(compare, a);
					Array.to_list(a);
				};
				List.iter(function n: var c = PMap.find(n, e.e_constrs);
						  p("\t%s", c.ef_name);
				switch (follow(c.ef_type)) {
			case TFun(args, _): p("[%s]", String.concat(", ", List.map(sparam, List.map(function (a, o, t): (new Tuple(a, if (o) {
					Some(Ident("null"));
					} else {
						None;
					}, t)), args))));
				case _: [];
				};
				p(";\n"), if (Meta.has(Meta.FakeEnum, e.e_meta)) {
				sort(e.e_names);
				} else {
					e.e_names;
				});
				p("}\n");
			case TTypeDecl(t): print_meta(t.t_meta);
				p("typedef %s = ", stype(TType(t, List.map(snd, t.t_params))));
				p("%s", stype(t.t_type));
				p("\n");
			case TAbstractDecl(a): print_meta(a.a_meta);
				p("abstract %s {}", stype(TAbstract(a, List.map(snd, a.a_params))));
			};
			IO.close_out(ch);
		};
	};

	public static function generate_hx(com) return {
		List.iter(generate_type(com), com.types);
	}
}
;
