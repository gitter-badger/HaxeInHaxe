import Printf;
import Ast;
import Genswf;
import Common;
import Type;

typedef Context = {
	com : Common.Context,
	flush : Unit -> Unit,
	setup : Unit -> Unit,
	messages : List<String>,
	has_next : Bool,
	has_error : Bool
};

typedef Cache = {
	c_haxelib : Hashtbl<String, String>,
	c_files : Hashtbl<String, Tuple<Float, Ast.Package>>,
	c_modules : Hashtbl<Tuple<Path, String>, Module_def>
};

class /*exception*/ Abort {

};

class /*exception*/ Completion {

};

class Main {
	public static var version = 3300;

	public static var version_major = / (version, 1000);

	public static var version_minor = / (mod(version, 1000), 100);

	public static var version_revision = mod(version, 100);

	public static var version_is_stable = = (land(version_minor, 1), 0);

	public static var measure_times = ref(False);

	public static var prompt = ref(False);

	public static var start_time = ref(get_time([]));

	public static var global_cache = ref(None);

	public static var path_sep = if ( = (Sys.os_type, "Unix")) {
		"/";
	} else {
		"\\";
	};

	public static function get_real_path(p) return {
		try {
			Extc.get_real_path(p);
		} catch (e: _) {
			p;
		};
	};

	public static function executable_path([]) return {
		Extc.executable_path([]);
	};

	public static function is_debug_run([]) return {
		try {
			= (Sys.getenv("HAXEDEBUG"), "1");
		} catch (e: _) {
			False;
		};
	};

	public static var s_version = Printf.sprintf("%d.%d.%d%s", version_major, version_minor, version_revision,
	switch (Version.version_extra) {
case None: "";
case Some(v): ^ (" ", v);
	});

	public static function format(msg, p) return {
		if ( = (p, Ast.null_pos)) {
			msg;
		} else {
			function error_printer(file, line) return {
				sprintf("%s:%d:", file, line);
			};
			var epos = Lexer.get_error_pos(error_printer, p);
			var msg = String.concat( ^ ("\n", ^ (epos, " : ")), ExtString.String.nsplit(msg, "\n"));
			sprintf("%s : %s", epos, msg);
		};
	};

	public static function ssend(sock, str) return {
		function loop(pos, len) return {
			if ( = (len, 0)) {
				[];
			} else {
				var s = Unix.send(sock, str, pos, len, []);
				loop(+(pos, s), -(len, s));
			};
		};
		loop(0, String.length(str));
	};

	public static function message(ctx, msg, p) return {
		ctx.messages = ::(format(msg, p), ctx.messages);
	};

	public static var deprecated = ::((new Tuple("Type not found : IntIter", "IntIter was renamed to IntIterator")),
									  ::((new Tuple("EReg has no field customReplace", "EReg.customReplace was renamed to EReg.map")),
										 ::((new Tuple("#StringTools has no field isEOF", "StringTools.isEOF was renamed to StringTools.isEof")),
											::((new Tuple("Type not found : haxe.BaseCode", "haxe.BaseCode was moved to haxe.crypto.BaseCode")),
													::((new Tuple("Type not found : haxe.Md5", "haxe.Md5 was moved to haxe.crypto.Md5")),
															::((new Tuple("Type not found : haxe.SHA1", "haxe.SHA1 was moved to haxe.crypto.SHA1")),
																	::((new Tuple("Type not found : Hash", "Hash has been removed, use Map instead")),
																			::((new Tuple("Type not found : IntHash", "IntHash has been removed, use Map instead")),
																					::((new Tuple("Type not found : haxe.FastList", "haxe.FastList was moved to haxe.ds.GenericStack")),
																							::((new Tuple("#Std has no field format",
																									"Std.format has been removed, use single quote 'string ${escape}' syntax instead")),
																									::((new Tuple("Identifier 'EType' is not part of enum haxe.macro.ExprDef", "EType has been removed, use EField instead")),
																											::((new Tuple("Identifier 'CType' is not part of enum haxe.macro.Constant", "CType has been removed, use CIdent instead")),
																													::((new Tuple("Type not found : haxe.rtti.Infos", "Use @:rtti instead of implementing haxe.rtti.Infos")),
																															::((new Tuple("Type not found : haxe.rtti.Generic", "Use @:generic instead of implementing haxe.Generic")),
																																	::((new Tuple("Type not found : flash.utils.TypedDictionary",
																																			"flash.utils.TypedDictionary has been removed, use Map instead")), ::((new Tuple("Type not found : haxe.Stack",
																																					"haxe.Stack has been renamed to haxe.CallStack")), ::((new Tuple("Type not found : neko.zip.Reader",
																																							"neko.zip.Reader has been removed, use haxe.zip.Reader instead")), ::((new Tuple("Type not found : neko.zip.Writer",
																																									"neko.zip.Writer has been removed, use haxe.zip.Writer instead")), ::((new Tuple("Type not found : haxe.Public",
																																											"Use @:publicFields instead of implementing or extending haxe.Public")), ::((new Tuple("#Xml has no field createProlog",
																																													"Xml.createProlog was renamed to Xml.createProcessingInstruction")), []))))))))))))))))))));

	public static function limit_string(s, offset) return {
		var rest = -(80, offset);
		var words = ExtString.String.nsplit(s, " ");
		function loop(i, words) return {
			switch (words) {
			case ::(word, words): if ( > (+(+(String.length(word), i), 1), rest)) {
					::(Printf.sprintf("\n%*s", offset, ""), ::(word, loop(String.length(word), words)));
				} else {
					::(if ( = (i, 0)) {
					"";
				} else {
					" ";
				}, ::(word, loop(+(+(i, 1), String.length(word)), words)));
				};
			case []: [];
			};
		};
		String.concat("", loop(0, words));
	};

	public static function error(ctx, msg, p) return {
		var msg = try {
			List.assoc(msg, deprecated);
		} catch (e: Not_found) {
			msg;
		};
		message(ctx, msg, p);
		ctx.has_error = True;
	};

	public static function htmlescape(s) return {
		var s = String.concat("&amp;", ExtString.String.nsplit(s, "&"));
		var s = String.concat("&lt;", ExtString.String.nsplit(s, "<"));
		var s = String.concat("&gt;", ExtString.String.nsplit(s, ">"));
		s;
	};

	public static var reserved_flags = ::("cross", ::("js", ::("neko", ::("flash", ::("php", ::("cpp", ::("cs", ::("java",
										  ::("python", ::("as3", ::("swc", ::("macro", ::("sys", [])))))))))))));

	public static function complete_fields(com, fields) return {
		var b = Buffer.create(0);
		var details = Common.raw_defined(com, "display-details");
		Buffer.add_string(b, "<list>\n");
		List.iter(function (n, t, k, d): var s_kind = switch (k) {
	case Some(k): switch (k) {
			case Typer.FKVar: "var";
			case Typer.FKMethod: "method";
			case Typer.FKType: "type";
			case Typer.FKPackage: "package";
			};
		case None: "";
		};
		if (details) {
		Buffer.add_string(b, Printf.sprintf("<i n=\"%s\" k=\"%s\"><t>%s</t><d>%s</d></i>\n", n, s_kind, htmlescape(t),
											htmlescape(d)));
		} else {
			Buffer.add_string(b, Printf.sprintf("<i n=\"%s\"><t>%s</t><d>%s</d></i>\n", n, htmlescape(t), htmlescape(d)));
		}, List.sort(function (a, _, ak, _): function (b, _, bk, _): compare((new Tuple(ak, a)), (new Tuple(bk, b))), fields));
		Buffer.add_string(b, "</list>\n");
		raise(Completion(Buffer.contents(b)));
	};

	public static function report_times(print) return {
		var tot = ref(0.);
		Hashtbl.iter(function _: function t: tot.val = +.(tot.val, t.total), Common.htimers);
		print(Printf.sprintf("Total time : %.3fs", tot.val));
		if ( > (tot.val, 0.)) {
			print("------------------------------------");
			var timers = List.sort(function t1: function t2: compare(t1.name, t2.name),
			Hashtbl.fold(function _: function t: function acc: ::(t, acc), Common.htimers, []));
			List.iter(function t: print(Printf.sprintf("  %s : %.3fs, %.0f%%", t.name, t.total, / .( * .(t.total, 100.), tot.val))),
			timers);
		} else {
			[];
		};
	};

	public static function make_path(f) return {
		var f = String.concat("/", ExtString.String.nsplit(f, "\\"));
		var cl = ExtString.String.nsplit(f, ".");
		var cl = switch (List.rev(cl)) {
		case ::(hx, ::(path, [])): ExtString.String.nsplit(path, "/");
		case _: cl;
		};
		function error(msg) return {
			var msg = ^ ("Could not process argument ", ^ (f, ^ ("\n", msg)));
			failwith(msg);
		};
		function invalid_char(x) return {
			for (i in /*to*/1... - (String.length(x), 1)) {
				switch (xi) {
				case 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_': [];
				case c: error( ^ ("invalid character: ", String.make(1, c)));
				};
			};
		};
		function loop(match) return switch (match) {
		case []: error("empty part");
		case ::(x, []): invalid_char(x);
			(new Tuple([], x));
		case ::(x, l): if ( = (String.length(x), 0)) {
				error("empty part");
			} else {
				if ( || (<(x0, 'a'), >(x0, 'z'))) {
					error("Package name must start with a lower case character");
				} else {
					[];
				};
			};
			invalid_char(x);
			var Tuple(path, name) = loop(l);
			(new Tuple(::(x, path), name));
		};
		loop(cl);
	};

	public static function starts_uppercase(x) return {
		|| ( = (x0, '_'), && ( >= (x0, 'A'), <= (x0, 'Z')));
	};

	public static function check_uppercase(x) return {
		if ( = (String.length(x), 0)) {
			failwith("empty part");
		} else {
			if (!(starts_uppercase(x))) {
				failwith("Class name must start with uppercase character");
			} else {
				[];
			};
		};
	};

	public static function make_type_path(f) return {
		var Tuple(pack, name) = make_path(f);
		check_uppercase(name);
		(new Tuple(pack, name));
	};

	public static function unique(l) return {
		function _unique(match) return switch (match) {
		case []: [];
		case ::(x1, ::(x2, l)) if (=(x1, x2)): _unique(::(x2, l));
		case ::(x, l): ::(x, _unique(l));
		};
		_unique(List.sort(compare, l));
	};

	public static function read_type_path(com, p) return {
		var classes = ref([]);
		var packages = ref([]);
		var p = switch (p) {
		case ::(x, l): try {
				switch (PMap.find(x, com.package_rules)) {
				case Directory(d): ::(d, l);
				case Remap(s): ::(s, l);
				case _: p;
				};
			} catch (e: Not_found) {
				p;
			};
		case _: p;
		};
		List.iter(function path: var dir = ^ (path, String.concat("/", p));
		var r = try {
			Sys.readdir(dir);
		} catch (e: _) {
			[];
		};
		Array.iter(function f:
		if (try {
			= (Unix.stat( ^ (dir, ^ ("/", f))).Unix.st_kind, Unix.S_DIR);
			} catch (e: _) {
				False;
			}) {
			if ( && ( >= (f0, 'a'), <= (f0, 'z'))) {
				if ( = (p, ::(".", []))) {
					switch (read_type_path(com, ::(f, []))) {
					case ([], []): [];
					case _: try {
							switch (PMap.find(f, com.package_rules)) {
							case Forbidden: [];
							case Remap(f): packages.val = ::(f, packages.val);
							case Directory(_): raise(Not_found);
							};
						} catch (e: Not_found) {
							packages.val = ::(f, packages.val);
						};
					};
				} else {
					packages.val = ::(f, packages.val);
				};
			} else {
				[];
			};
		} else {
			if ( = (file_extension(f), "hx")) {
				var c = Filename.chop_extension(f);
				if ( || ( < (String.length(c), 2), <>(String.sub(c, -(String.length(c), 2), 2), "__"))) {
					classes.val = ::(c, classes.val);
				} else {
					[];
				};
			} else {
				[];
			};
		}, r), com.class_path);
		List.iter(function (_, _, extract): Hashtbl.iter(function (path, name): function _:
		if ( = (path, p)) {
		classes.val = ::(name, classes.val);
		} else {
			function loop(p1, p2) return {
				switch ((new Tuple(p1, p2))) {
				case ([], _): [];
				case (::(x, _), []): packages.val = ::(x, packages.val);
				case (::(a, p1), ::(b, p2)): if ( = (a, b)) {
						loop(p1, p2);
					} else {
						[];
					};
				};
			};
			loop(path, p);
		}, extract([])), com.swf_libs);
		List.iter(function (path, std, close, all_files, lookup): List.iter(function (path, name):
		if ( = (path, p)) {
		classes.val = ::(name, classes.val);
		} else {
			function loop(p1, p2) return {
				switch ((new Tuple(p1, p2))) {
				case ([], _): [];
				case (::(x, _), []): packages.val = ::(x, packages.val);
				case (::(a, p1), ::(b, p2)): if ( = (a, b)) {
						loop(p1, p2);
					} else {
						[];
					};
				};
			};
			loop(path, p);
		}, all_files([])), com.java_libs);
		List.iter(function (path, std, all_files, lookup): List.iter(function (path, name):
		if ( = (path, p)) {
		classes.val = ::(name, classes.val);
		} else {
			function loop(p1, p2) return {
				switch ((new Tuple(p1, p2))) {
				case ([], _): [];
				case (::(x, _), []): packages.val = ::(x, packages.val);
				case (::(a, p1), ::(b, p2)): if ( = (a, b)) {
						loop(p1, p2);
					} else {
						[];
					};
				};
			};
			loop(path, p);
		}, all_files([])), com.net_libs);
		(new Tuple(unique(packages.val), unique(classes.val)));
	};

	public static function delete_file(f) return {
		try {
			Sys.remove(f);
		} catch (e: _) {
			[];
		};
	};

	public static function expand_env( ? : (h = None), path) return {
		var r = Str.regexp("%\\[[A-Za-z0-9_]+\\]%");
		Str.global_substitute(r, function s: var key = Str.matched_group(1, s);
		try {
			Sys.getenv(key);
		} catch (e: Not_found) {
			try {
				switch (h) {
				case None: raise(Not_found);
				case Some(h): Hashtbl.find(h, key);
				};
			} catch (e: Not_found) {
				^ ("%", ^ (key, "%"));
			};
		}, path);
	};

	public static function unquote(v) return {
		var len = String.length(v);
		if ( && ( > (len, 0), && ( = (v0, '"'), = (v - (len, 1), '"')))) {
			String.sub(v, 1, -(len, 2));
		} else {
			v;
		};
	};

	public static function parse_hxml_data(data) return {
		var lines = Str.split(Str.regexp("[\r\n]+"), data);
		List.concat(List.map(function l: var l = unquote(ExtString.String.strip(l));
		if ( || ( = (l, ""), = (l0, '#'))) {
		[];
		} else {
			if ( = (l0, '-')) {
				try {
					var Tuple(a, b) = ExtString.String.split(l, " ");
					::(unquote(a), ::(unquote(ExtString.String.strip(b)), []));
				} catch (e: _) {
					::(l, []);
				};
			} else {
				::(l, []);
			};
		}, lines));
	};

	public static function parse_hxml(file) return {
		var ch = IO.input_channel(try {
			open_in_bin(file);
		} catch (e: _) {
			raise(Not_found);
		});
		var data = IO.read_all(ch);
		IO.close_in(ch);
		parse_hxml_data(data);
	};

	public static function lookup_classes(com, spath) return {
		function loop(match) return switch (match) {
		case []: [];
		case ::(cp, l): var cp = if ( = (cp, "")) {
				"./";
			} else {
				cp;
			};
			var c = normalize_path(get_real_path(Common.unique_full_path(cp)));
			var clen = String.length(c);
			if ( && ( < (clen, String.length(spath)), = (String.sub(spath, 0, clen), c))) {
				var path = String.sub(spath, clen, -(String.length(spath), clen));
				try {
					var path = make_type_path(path);
					switch (loop(l)) {
					case ::(x, []) if (<(String.length(Ast.s_type_path(x)), String.length(Ast.s_type_path(path)))): ::(x, []);
					case _: ::(path, []);
					};
				} catch (e: _) {
					loop(l);
				};
			} else {
				loop(l);
			};
		};
		loop(com.class_path);
	};

	public static function add_libs(com, libs) return {
		function call_haxelib([]) return {
			var t = Common.timer("haxelib");
			var cmd = ^ ("haxelib path ", String.concat(" ", libs));
			var Tuple(pin, pout, perr) = Unix.open_process_full(cmd, Unix.environment([]));
			var lines = Std.input_list(pin);
			var err = Std.input_list(perr);
			var ret = Unix.close_process_full((new Tuple(pin, pout, perr)));
			if (<>(ret, Unix.WEXITED(0))) {
				failwith(switch ((new Tuple(lines, err))) {
			case ([], []): "Failed to call haxelib [command not found ?]";
				case ([], ::(s, [])) if (ExtString.String.ends_with(ExtString.String.strip(s), "Module not found : path")):
					"The haxelib command has been strip'ed, please install it again";
				case _: String.concat("\n", @(lines, err));
				});
			} else {
				[];
			};
			t([]);
			lines;
		};
		switch (libs) {
		case []: [];
		case _: var lines = switch (global_cache.val) {
			case Some(cache): try {
					if ( = (com.display, DMNone)) {
						raise(Not_found);
					} else {
						[];
					};
					Hashtbl.find(cache.c_haxelib, libs);
				} catch (e: Not_found) {
					var lines = call_haxelib([]);
					Hashtbl.replace(cache.c_haxelib, libs, lines);
					lines;
				};
			case _: call_haxelib([]);
			};
			var extra_args = ref([]);
			var lines = List.fold_left(function acc: function l: var l = ExtString.String.strip(l);
			if ( = (l, "")) {
			acc;
		} else {
			if (<>(l0, '-')) {
					::(l, acc);
				} else {
					switch (try {
						ExtString.String.split(l, " ");
						} catch (e: _) {
							(new Tuple(l, ""));
						}) {
					case (-L, dir): com.neko_libs = ::(String.sub(l, 3, -(String.length(l), 3)), com.neko_libs);
						acc;
					case (param, value): extra_args.val = ::(param, extra_args.val);
						if (<>(value, "")) {
							extra_args.val = ::(value, extra_args.val);
						} else {
							[];
						};
						acc;
					};
				};
			}, [], lines);
			com.class_path = @(lines, com.class_path);
			List.rev(extra_args.val);
		};
	};

	public static function run_command(ctx, cmd) return {
		var h = Hashtbl.create(0);
		Hashtbl.add(h, "__file__", ctx.com.file);
		Hashtbl.add(h, "__platform__", platform_name(ctx.com.platform));
		var t = Common.timer("command");
		var cmd = expand_env(h = Some(h), cmd);
		var len = String.length(cmd);
		if ( && ( > (len, 3), = (String.sub(cmd, 0, 3), "cd "))) {
			Sys.chdir(String.sub(cmd, 3, -(len, 3)));
			0;
		} else {
			function binary_string(s) return {
				if ( && (<>(Sys.os_type, "Win32"), <>(Sys.os_type, "Cygwin"))) {
					s;
				} else {
					String.concat("\n", Str.split(Str.regexp("\r\n"), s));
				};
			};
			var Tuple(pout, pin, perr) = Unix.open_process_full(cmd, Unix.environment([]));
			var iout = Unix.descr_of_in_channel(pout);
			var ierr = Unix.descr_of_in_channel(perr);
			var berr = Buffer.create(0);
			var bout = Buffer.create(0);
			var tmp = String.create(1024);
			var result = ref(None);
			function is_process_running([]) return {
				var Tuple(pid, r) = Unix.waitpid(::(Unix.WNOHANG, []), -1);
				if ( = (pid, 0)) {
					True;
				} else {
					result.val = Some(r);
					False;
				};
			};
			function loop(ins) return {
				var Tuple(Tuple(ch, _, _), timeout) = try {
					(new Tuple(Unix.select(ins, [], [], 0.02), True));
				} catch (e: _) {
					(new Tuple((new Tuple([], [], [])), False));
				};
				switch (ch) {
				case []: if ( && (timeout, is_process_running([]))) {
						loop(ins);
					} else {
						Buffer.add_string(berr, IO.read_all(IO.input_channel(perr)));
						Buffer.add_string(bout, IO.read_all(IO.input_channel(pout)));
					};
				case ::(s, _): var n = Unix.read(s, tmp, 0, String.length(tmp));
					if ( && ( == (s, iout), > (n, 0))) {
						ctx.com.print(String.sub(tmp, 0, n));
					} else {
						Buffer.add_substring(if ( == (s, iout)) {
						bout;
					} else {
						berr;
					}, tmp, 0, n);
					};
					loop(if ( = (n, 0)) {
					List.filter(s !=, ins);
					} else {
						ins;
					});
				};
			};
			try {
				loop(::(iout, ::(ierr, [])));
			} catch (e: Unix.Unix_error(_)) {
				[];
			};
			var serr = binary_string(Buffer.contents(berr));
			var sout = binary_string(Buffer.contents(bout));
			if (<>(serr, "")) {
				ctx.messages = ::(if ( = (serr - (String.length(serr), 1), '\n')) {
				String.sub(serr, 0, -(String.length(serr), 1));
				} else {
					serr;
				}, ctx.messages);
			} else {
				[];
			};
			if (<>(sout, "")) {
				ctx.com.print(sout);
			} else {
				[];
			};
			var r = switch (try {
				Unix.close_process_full((new Tuple(pout, pin, perr)));
				} catch (e: Unix.Unix_error(Unix.ECHILD)(_)(_)) {
					switch (result.val) {
					case None: assert False;
					case Some(r): r;
					};
				}) {
			case Unix.WEXITED(e): e;
			case Unix.WSIGNALED(s) | Unix.WSTOPPED(s): if ( = (s, 0)) {
					-1;
				} else {
					s;
				};
			};
			t([]);
			r;
		};
	};

	public static function display_memory(ctx) return {
		var verbose = ctx.com.verbose;
		var print = print_endline;
		function fmt_size(sz) return {
			if ( < (sz, 1024)) {
				^ (string_of_int(sz), " B");
			} else {
				if ( < (sz, * (1024, 1024))) {
					^ (string_of_int(asr(sz, 10)), " KB");
				} else {
					Printf.sprintf("%.1f MB", / .(float_of_int(sz), * .(1024., 1024.)));
				};
			};
		};
		function size(v) return {
			fmt_size(mem_size(v));
		};
		Gc.full_major([]);
		Gc.compact([]);
		var mem = Gc.stat([]);
		print( ^ ("Total Allocated Memory ", fmt_size( * (mem.Gc.heap_words, asr(Sys.word_size, 8)))));
		print( ^ ("Free Memory ", fmt_size( * (mem.Gc.free_words, asr(Sys.word_size, 8)))));
		switch (global_cache.val) {
		case None: print("No cache found");
		case Some(c): print( ^ ("Total cache size ", size(c)));
			print( ^ ("  haxelib ", size(c.c_haxelib)));
			print( ^ ("  parsed ast ", ^ (size(c.c_files), ^ (", [", ^ (string_of_int(Hashtbl.length(c.c_files)),
										  " files stored]")))));
			print( ^ ("  typed modules ", ^ (size(c.c_modules), ^ (", [", ^ (string_of_int(Hashtbl.length(c.c_modules)),
											 " modules stored]")))));
			function scan_module_deps(m, h) return {
				if (Hashtbl.mem(h, m.m_id)) {
					[];
				} else {
					Hashtbl.add(h, m.m_id, m);
					PMap.iter(function _: function m: scan_module_deps(m, h), m.m_extra.m_deps);
				};
			};
			var all_modules = Hashtbl.fold(function _: function m: function acc: PMap.add(m.m_id, m, acc), c.c_modules, PMap.empty);
			var modules = Hashtbl.fold(function (path, key): function m: function acc: var mdeps = Hashtbl.create(0);
									   scan_module_deps(m, mdeps);
									   var deps = ref([]);
									   var out = ref(all_modules);
									   Hashtbl.iter(function _: function md: out.val = PMap.remove(md.m_id, out.val);
			if ( == (m, md)) {
			[];
			} else {
				deps.val = ::(Obj.repr(md), deps.val);
				List.iter(function t:
				switch (t) {
			case TClassDecl(c): deps.val = ::(Obj.repr(c), deps.val);
					List.iter(function f: deps.val = ::(Obj.repr(f), deps.val), c.cl_ordered_statics);
					List.iter(function f: deps.val = ::(Obj.repr(f), deps.val), c.cl_ordered_fields);
				case TEnumDecl(e): deps.val = ::(Obj.repr(e), deps.val);
					List.iter(function n: deps.val = ::(Obj.repr(PMap.find(n, e.e_constrs)), deps.val), e.e_names);
				case TTypeDecl(t): deps.val = ::(Obj.repr(t), deps.val);
				case TAbstractDecl(a): deps.val = ::(Obj.repr(a), deps.val);
				}, md.m_types);
			}, mdeps);
			var chk = ::(Obj.repr(Common.memory_marker), PMap.fold(function m: function acc: ::(Obj.repr(m), acc), out.val, []));
					  var inf = Objsize.objsize(m, deps.val, chk);
					  ::((new Tuple(m, Objsize.size_with_headers(inf), (new Tuple(inf.Objsize.reached, deps.val, out.val)))), acc),
					  c.c_modules, []);
			var cur_key = ref("");
			var tcount = ref(0);
			var mcount = ref(0);
			List.iter(function (m, size, (reached, deps, out)): var key = m.m_extra.m_sign;
			if (<>(key, cur_key.val)) {
			print(Printf.sprintf("    --- CONFIG %s ----------------------------", Digest.to_hex(key)));
				cur_key.val = key;
			} else {
				[];
			};
			function sign(md) return {
				if ( = (md.m_extra.m_sign, key)) {
					"";
				} else {
					^ ("[", ^ (try {
						Digest.to_hex(md.m_extra.m_sign);
					} catch (e: _) {
						^ ("???", md.m_extra.m_sign);
					}, "]"));
				};
			};
			print(Printf.sprintf("    %s : %s", Ast.s_type_path(m.m_path), fmt_size(size)));
			if (reached) {
			try {
				incr(mcount);
					var lcount = ref(0);
					function leak(l) return {
						incr(lcount);
						incr(tcount);
						print(Printf.sprintf("      LEAK %s", l));
						if ( && ( >= (lcount.val, 3), && ( >= (tcount.val, 100), !(verbose)))) {
							print(Printf.sprintf("      ..."));
							raise(Exit);
						} else {
							[];
						};
					};
					if (Objsize.objsize(m, deps, ::(Obj.repr(Common.memory_marker), [])).Objsize.reached) {
						leak("common");
					} else {
						[];
					};
					PMap.iter(function _: function md:
					if (Objsize.objsize(m, deps, ::(Obj.repr(md), [])).Objsize.reached) {
					leak( ^ (Ast.s_type_path(md.m_path), sign(md)));
					} else {
						[];
					}, out);
				} catch (e: Exit) {
					[];
				};
			} else {
				[];
			};
			if (verbose) {
			print(Printf.sprintf("      %d total deps", List.length(deps)));
				PMap.iter(function _: function md: print(Printf.sprintf("      dep %s%s", Ast.s_type_path(md.m_path), sign(md))),
						  m.m_extra.m_deps);
			} else {
				[];
			};
			flush(stdout), List.sort(function (m1, s1, _): function (m2, s2, _): var k1 = m1.m_extra.m_sign;
									 var k2 = m2.m_extra.m_sign;
			if ( = (k1, k2)) {
			-(s1, s2);
			} else {
				if ( > (k1, k2)) {
					1;
				} else {
					-1;
				};
			}, modules));
			if ( > (mcount.val, 0)) {
				print( ^ ("*** ", ^ (string_of_int(mcount.val), " modules have leaks !")));
			} else {
				[];
			};
			print("Cache dump complete");
		};
	};

	public static function default_flush(ctx) return {
		List.iter(prerr_endline, List.rev(ctx.messages));
		if ( && (ctx.has_error, prompt.val)) {
			print_endline("Press enter to exit...");
			ignore(read_line([]));
		} else {
			[];
		};
		if (ctx.has_error) {
			exit(1);
		} else {
			[];
		};
	};

	public static function create_context(params) return {
		var ctx = {
			() with com = Common.create(version, params);
			flush = function []: [];
			setup = function []: [];
			messages = [];
			has_next = False;
			has_error = False
		};
		ctx.flush = function []: default_flush(ctx);
		ctx;
	};

	public static function process_params(create, pl) return {
		var each_params = ref([]);
		function loop(acc) return {
		case []: var ctx = create(@(each_params.val, List.rev(acc)));
			init(ctx);
			ctx.flush([]);
		case ::(--next, l) if (=(acc, [])): loop([], l);
		case ::(--next, l): var ctx = create(@(each_params.val, List.rev(acc)));
			ctx.has_next = True;
			init(ctx);
			ctx.flush([]);
			loop([], l);
		case ::(--each, l): each_params.val = List.rev(acc);
			loop([], l);
		case ::(--cwd, ::(dir, l)): try {
				Unix.chdir(dir);
			} catch (e: _) {
				raise(Arg.Bad("Invalid directory"));
			};
			loop(acc, l);
		case ::(--connect, ::(hp, l)): switch (global_cache.val) {
			case None: var Tuple(host, port) = try {
					ExtString.String.split(hp, ":");
				} catch (e: _) {
					(new Tuple("127.0.0.1", hp));
				};
				do_connect(host, try {
					int_of_string(port);
				} catch (e: _) {
					raise(Arg.Bad("Invalid port"));
				}, @(List.rev(acc), l));
			case Some(_): loop(acc, l);
			};
		case ::(--run, ::(cl, args)): var acc = ::(cl, ::("-main", ::("--interp", acc)));
			var ctx = create(@(each_params.val, List.rev(acc)));
			ctx.com.sys_args = args;
			init(ctx);
			ctx.flush([]);
		case ::(arg, l): switch (List.rev(ExtString.String.nsplit(arg, "."))) {
			case ::(hxml, _) if (switch (acc) {
					case ::(-cmd, _): False;
						case _: True;
						}): var Tuple(acc, l) = try {
					(new Tuple(acc, @(parse_hxml(arg), l)));
				} catch (e: Not_found) {
					(new Tuple(::( ^ (arg, ", [file not found]"), acc), l));
				};
				loop(acc, l);
			case _: loop(::(arg, acc), l);
			};
		};
		var pl = switch (List.rev(pl)) {
		case ::(file, ::(--display, pl)) if (<>(file, "memory")): ::("--display", ::(file, List.rev(pl)));
		case ::(use_rtti_doc, ::(-D, ::(file, ::(--display, pl)))): ::("--display", ::(file, List.rev(pl)));
		case _: pl;
		};
		loop([], pl);
	};

	public static function wait_loop(boot_com, host, port) return {
		var sock = Unix.socket(Unix.PF_INET, Unix.SOCK_STREAM, 0);
		try {
			Unix.setsockopt(sock, Unix.SO_REUSEADDR, True);
		} catch (e: _) {
			[];
		};
		try {
			Unix.bind(sock, Unix.ADDR_INET(Unix.inet_addr_of_string(host), port));
		} catch (e: _) {
			failwith( ^ ("Couldn't wait on ", ^ (host, ^ (":", string_of_int(port)))));
		};
		Unix.listen(sock, 10);
		Sys.catch_break(False);
		var verbose = boot_com.verbose;
		var has_parse_error = ref(False);
		if (verbose) {
			print_endline( ^ ("Waiting on ", ^ (host, ^ (":", string_of_int(port)))));
		} else {
			[];
		};
		var bufsize = 1024;
		var tmp = String.create(bufsize);
		var cache = { () with c_haxelib = Hashtbl.create(0);
					  c_files = Hashtbl.create(0);
					  c_modules = Hashtbl.create(0)
					};
		global_cache.val = Some(cache);
		Typer.macro_enable_cache.val = True;
		Typeload.parse_hook.val = function com2: function file: function p: var sign = get_signature(com2);
		var ffile = Common.unique_full_path(file);
		var ftime = file_time(ffile);
		var fkey = ^ (ffile, ^ ("!", sign));
		try {
			var Tuple(time, data) = Hashtbl.find(cache.c_files, fkey);
			if (<>(time, ftime)) {
				raise(Not_found);
			} else {
				[];
			};
			data;
		} catch (e: Not_found) {
			has_parse_error.val = False;
			var data = Typeload.parse_file(com2, file, p);
			if (verbose) {
				print_endline( ^ ("Parsed ", ffile));
			} else {
				[];
			};
			if ( && (!(has_parse_error.val), <>(ffile, Parser.resume_display.val.Ast.pfile))) {
				Hashtbl.replace(cache.c_files, fkey, (new Tuple(ftime, data)));
			} else {
				[];
			};
			data;
		};
		function cache_module(m) return {
			Hashtbl.replace(cache.c_modules, (new Tuple(m.m_path, m.m_extra.m_sign)), m);
		};
		function check_module_path(com, m, p) return {
			if (<>(m.m_extra.m_file, Common.unique_full_path(Typeload.resolve_module_file(com, m.m_path, ref([]), p)))) {
				if (verbose) {
					print_endline( ^ ("Module path ", ^ (s_type_path(m.m_path), " has been changed")));
				} else {
					[];
				};
				raise(Not_found);
			} else {
				[];
			};
		};
		var compilation_step = ref(0);
		var compilation_mark = ref(0);
		var mark_loop = ref(0);
		Typeload.type_module_hook.val = function ctxTypecore.typer: function mpath: function p: var t = Common.timer("module cache check");
		var com2 = ctx.Typecore.com;
		var sign = get_signature(com2);
		var dep = ref(None);
		incr(mark_loop);
		var mark = mark_loop.val;
		var start_mark = compilation_mark.val;
		function check(m) return {
			if (m.m_extra.m_dirty) {
				dep.val = Some(m);
				False;
			} else {
				if ( = (m.m_extra.m_mark, mark)) {
					True;
				} else {
					try {
						if ( <= (m.m_extra.m_mark, start_mark)) {
							switch (m.m_extra.m_kind) {
							case MFake | MSub: [];
							case MExtern: var has_file = try {
									ignore(Typeload.resolve_module_file(com2, m.m_path, ref([]), p));
									True;
								} catch (e: Not_found) {
									False;
								};
								if (has_file) {
									if (verbose) {
										print_endline( ^ ("A file is masking the library file ", s_type_path(m.m_path)));
									} else {
										[];
									};
									raise(Not_found);
								} else {
									[];
								};
								function loop(match) return switch (match) {
								case []: if (verbose) {
										print_endline( ^ ("No library file was found for ", s_type_path(m.m_path)));
									} else {
										[];
									};
									raise(Not_found);
								case ::(load, l): switch (load(m.m_path, p)) {
									case None: loop(l);
									case Some(file, _): if (<>(Common.unique_full_path(file), m.m_extra.m_file)) {
											if (verbose) {
												print_endline( ^ ("Library file was changed for ", s_type_path(m.m_path)));
											} else {
												[];
											};
											raise(Not_found);
										} else {
											[];
										};
									};
								};
								loop(com2.load_extern_type);
							case MCode: check_module_path(com2, m, p);
							case MMacro if (ctx.Typecore.in_macro): check_module_path(com2, m, p);
							case MMacro: var Tuple(_, mctx) = Typer.get_macro_context(ctx, p);
								check_module_path(mctx.Typecore.com, m, p);
							};
							if (<>(file_time(m.m_extra.m_file), m.m_extra.m_time)) {
								if (verbose) {
									print_endline( ^ ("File ", ^ (m.m_extra.m_file, if ( = (m.m_extra.m_time, -1.)) {
									" not cached [macro-in-macro]";
								} else {
									" has been modified";
								})));
								} else {
									[];
								};
								if ( = (m.m_extra.m_kind, MFake)) {
									Hashtbl.remove(Typecore.fake_modules, m.m_extra.m_file);
								} else {
									[];
								};
								raise(Not_found);
							} else {
								[];
							};
						} else {
							[];
						};
						m.m_extra.m_mark = mark;
						PMap.iter(function _: function m2:
						if (!(check(m2))) {
						dep.val = Some(m2);
							raise(Not_found);
						} else {
							[];
						}, m.m_extra.m_deps);
						True;
					} catch (e: Not_found) {
						m.m_extra.m_dirty = True;
						False;
					};
				};
			};
		};
		function add_modules(m0, m) return {
			if ( < (m.m_extra.m_added, compilation_step.val)) {
				switch ((new Tuple(m0.m_extra.m_kind, m.m_extra.m_kind))) {
				case (MCode, MMacro) | (MMacro, MCode): [];
				case _: if (verbose) {
						print_endline( ^ ("Reusing  cached module ", Ast.s_type_path(m.m_path)));
					} else {
						[];
					};
					m.m_extra.m_added = compilation_step.val;
					List.iter(function t:
					switch (t) {
				case TClassDecl(c): c.cl_restore([]);
					case TEnumDecl(e): 	function loop(acc) return {
						case []: [];
						case ::((Ast.Meta.RealPath, ::((Ast.EConst(Ast.String(path)), _), []), _), l): e.e_path = Ast.parse_path(path);
							e.e_meta = @(List.rev(acc), l);
						case ::(x, l): loop(::(x, acc), l);
						};
						loop([], e.e_meta);
					case TAbstractDecl(a): a.a_meta = List.filter(function (m, _, _): <>(m, Ast.Meta.ValueUsed), a.a_meta);
					case _: [];
					}, m.m_types);
					if (<>(m.m_extra.m_kind, MSub)) {
						Typeload.add_module(ctx, m, p);
					} else {
						[];
					};
					PMap.iter(Hashtbl.add(com2.resources), m.m_extra.m_binded_res);
					PMap.iter(function _: function m2: add_modules(m0, m2), m.m_extra.m_deps);
				};
				List.iter(Typer.call_init_macro(ctx), m.m_extra.m_macro_calls);
			} else {
				[];
			};
		};
		try {
			var m = Hashtbl.find(cache.c_modules, (new Tuple(mpath, sign)));
			if (!(check(m))) {
				if (verbose) {
					print_endline( ^ ("Skipping cached module ", ^ (Ast.s_type_path(mpath), switch (dep.val) {
				case None: "";
				case Some(m): ^ ("[", ^ (Ast.s_type_path(m.m_path), "]"));
					})));
				} else {
					[];
				};
				raise(Not_found);
			} else {
				[];
			};
			add_modules(m, m);
			t([]);
			Some(m);
		} catch (e: Not_found) {
			t([]);
			None;
		};
		var run_count = ref(0);
		Truevar Tuple(sin, _) = Unix.accept(sock);
		var t0 = get_time([]);
		Unix.set_nonblock(sin);
		if (verbose) {
			print_endline("Client connected");
		} else {
			[];
		};
		var b = Buffer.create(0);
		function read_loop(count) return {
			var r = try {
				Unix.recv(sin, tmp, 0, bufsize, []);
			} catch (e: Unix.Unix_error(Unix.EWOULDBLOCK | Unix.EAGAIN)(_)(_)) {
				0;
			};
			if (verbose) {
				if ( > (r, 0)) {
					Printf.printf("Reading %d bytes\n", r);
				} else {
					print_endline("Waiting for data...");
				};
			} else {
				[];
			};
			Buffer.add_substring(b, tmp, 0, r);
			if ( && ( > (r, 0), = (tmp - (r, 1), '\000'))) {
				Buffer.sub(b, 0, -(Buffer.length(b), 1));
			} else {
				if ( = (r, 0)) {
					ignore(Unix.select([], [], [], 0.05));
				} else {
					[];
				};
				if ( = (count, 100)) {
					failwith("Aborting unactive connection");
				} else {
					read_loop(+(count, 1));
				};
			};
		};
		function cache_context(com) return {
			if ( = (com.display, DMNone)) {
				List.iter(cache_module, com.modules);
				if (verbose) {
					print_endline( ^ ("Cached ", ^ (string_of_int(List.length(com.modules)), " modules")));
				} else {
					[];
				};
			} else {
				[];
			};
			switch (com.get_macros([])) {
			case None: [];
			case Some(com): cache_context(com);
			};
		};
		function create(params) return {
			var ctx = create_context(params);
			ctx.flush = function []: incr(compilation_step);
			compilation_mark.val = mark_loop.val;
			List.iter(function s: ssend(sin, ^ (s, "\n"));
			if (verbose) {
			print_endline( ^ ("> ", s));
			} else {
				[];
			}, List.rev(ctx.messages));
			if (ctx.has_error) {
				ssend(sin, "\x02\n");
			} else {
				cache_context(ctx.com);
			};
			ctx.setup = function []: Parser.display_error.val = function e: function p: has_parse_error.val = True;
			ctx.com.error(Parser.error_msg(e), p);
			if (<>(ctx.com.display, DMNone)) {
				var file = Parser.resume_display.val.Ast.pfile;
				var fkey = ^ (file, ^ ("!", get_signature(ctx.com)));
				Hashtbl.remove(cache.c_files, fkey);
				Hashtbl.iter(function _: function m:
				if ( = (m.m_extra.m_file, file)) {
				m.m_extra.m_dirty = True;
			} else {
				[];
				}, cache.c_modules);
			} else {
				[];
			};
			ctx.com.print = function str: ssend(sin, ^ ("\x01", ^ (String.concat("\x01", ExtString.String.nsplit(str, "\n")), "\n")));
			ctx;
		};
		try {
			var data = parse_hxml_data(read_loop(0));
			Unix.clear_nonblock(sin);
			if (verbose) {
				print_endline( ^ ("Processing Arguments [", ^ (String.concat(",", data), "]")));
			} else {
				[];
			};
			try {
				Common.display_default.val = DMNone;
				Parser.resume_display.val = Ast.null_pos;
				Typeload.return_partial_type.val = False;
				measure_times.val = False;
				close_times([]);
				stats.s_files_parsed.val = 0;
				stats.s_classes_built.val = 0;
				stats.s_methods_typed.val = 0;
				stats.s_macros_called.val = 0;
				Hashtbl.clear(Common.htimers);
				var _ = Common.timer("other");
				incr(compilation_step);
				compilation_mark.val = mark_loop.val;
				start_time.val = get_time([]);
				process_params(create, data);
				close_times([]);
				if (measure_times.val) {
					report_times(function s: ssend(sin, ^ (s, "\n")));
				} else {
					[];
				};
			} catch (e: Completion(str)) {
				if (verbose) {
					print_endline( ^ ("Completion Response =\n", str));
				} else {
					[];
				};
				ssend(sin, str);
			};
			if (verbose) {
				print_endline(Printf.sprintf("Stats = %d files, %d classes, %d methods, %d macros", stats.s_files_parsed.val,
											 stats.s_classes_built.val, stats.s_methods_typed.val, stats.s_macros_called.val));
				print_endline(Printf.sprintf("Time spent : %.3fs", -.(get_time([]), t0)));
			} else {
				[];
			};
		} catch (e: T) {
			McOr(McArr(PaApp(PaId(IdAcc(<...>, <...>)), PaAny), ExNil, ExIfe(ExId(IdLid(<...>)), ExApp(ExId(<...>), ExStr(<...>)),
					   ExId(IdUid(<...>)))), McArr(PaId(IdLid(e)), ExNil, ExLet(ReNil, BiEq(PaId(<...>), ExApp(<...>, <...>)), ExSeq(ExSem(<...>,
											   <...>)))))			case Unix.Unix_error(_): if (verbose) {
				print_endline("Connection Aborted");
			} else {
				[];
			};
		case e: var estr = Printexc.to_string(e);
			if (verbose) {
				print_endline( ^ ("Uncaught Error : ", estr));
			} else {
				[];
			};
			try {
				ssend(sin, estr);
			} catch (e: _) {
				[];
			};
		};
		Unix.close(sin);
		incr(run_count);
		if ( = (mod(run_count.val, 10), 0)) {
			var t0 = get_time([]);
			Gc.compact([]);
			if (verbose) {
				var stat = Gc.quick_stat([]);
				var size = * .(float_of_int(stat.Gc.heap_words), 4.);
				print_endline(Printf.sprintf("Compacted memory %.3fs %.1fMB", -.(get_time([]), t0), / .(size, * .(1024., 1024.))));
			} else {
				[];
			};
		} else {
			Gc.minor([]);
		};
	};

	public static function do_connect(host, port, args) return {
		var sock = Unix.socket(Unix.PF_INET, Unix.SOCK_STREAM, 0);
		try {
			Unix.connect(sock, Unix.ADDR_INET(Unix.inet_addr_of_string(host), port));
		} catch (e: _) {
			failwith( ^ ("Couldn't connect on ", ^ (host, ^ (":", string_of_int(port)))));
		};
		var args = ::( ^ ("--cwd ", Unix.getcwd([])), args);
		ssend(sock, ^ (String.concat("", List.map(function a: ^ (a, "\n"), args)), "\000"));
		var has_error = ref(False);
		function print(line) return {
			switch (if ( = (line, "")) {
				'\x00';
			} else {
				line0;
			}) {
			case '\x01': print_string(String.concat("\n", List.tl(ExtString.String.nsplit(line, "\x01"))));
				flush(stdout);
			case '\x02': has_error.val = True;
			case _: prerr_endline(line);
			};
		};
		var buf = Buffer.create(0);
		function process([]) return {
			var lines = ExtString.String.nsplit(Buffer.contents(buf), "\n");
			var lines = switch (List.rev(lines)) {
			case ::(, l): List.rev(l);
			case _: lines;
			};
			List.iter(print, lines);
		};
		var tmp = String.create(1024);
		function loop([]) return {
			var b = Unix.recv(sock, tmp, 0, 1024, []);
			Buffer.add_substring(buf, tmp, 0, b);
			if ( > (b, 0)) {
				if ( = (String.get(tmp, -(b, 1)), '\n')) {
					process([]);
					Buffer.reset(buf);
				} else {
					[];
				};
				loop([]);
			} else {
				[];
			};
		};
		loop([]);
		process([]);
		if (has_error.val) {
			exit(1);
		} else {
			[];
		};
	};

	public static function init(ctx) return {
		var usage = Printf.sprintf("Haxe Compiler %s - [C]2005-2015 Haxe Foundation\n Usage : haxe%s -main <class> [-swf|-js|-neko|-php|-cpp|-as3|-cs|-java|-python] <output> [options]\n Options :", s_version, if ( = (Sys.os_type, "Win32")) {
		".exe";
	} else {
		"";
	});
		var com = ctx.com;
		var classes = ref(::((new Tuple([], "Std")), []));
		try {
			var xml_out = ref(None);
			var swf_header = ref(None);
			var cmds = ref([]);
			var config_macros = ref([]);
			var cp_libs = ref([]);
			var added_libs = Hashtbl.create(0);
			var no_output = ref(False);
			var did_something = ref(False);
			var force_typing = ref(False);
			var pre_compilation = ref([]);
			var interp = ref(False);
			var swf_version = ref(False);
			var evals = ref([]);
			Common.define_value(com, Define.HaxeVer, float_repres( / .(float_of_int(version), 1000.)));
			Common.define_value(com, Define.HxcppApiLevel, "321");
			Common.raw_define(com, "haxe3");
			Common.define_value(com, Define.Dce, "std");
			com.warning = function msg: function p: message(ctx, ^ ("Warning : ", msg), p);
			com.error = error(ctx);
			if (<>(global_cache.val, None)) {
				com.run_command = run_command(ctx);
			} else {
				[];
			};
			Parser.display_error.val = function e: function p: com.error(Parser.error_msg(e), p);
			Parser.use_doc.val = || (<>(Common.display_default.val, DMNone), <>(global_cache.val, None));
			try {
				var p = Sys.getenv("HAXE_STD_PATH");
				function loop(match) return switch (match) {
				case ::(drive, ::(path, l)): if ( && ( = (String.length(drive), 1), || ( && ( >= (drive0, 'a'), <= (drive0, 'z')),
					&& ( >= (drive0, 'A'), <= (drive0, 'Z'))))) {
						::( ^ (drive, ^ (":", path)), loop(l));
					} else {
						::(drive, loop(::(path, l)));
					};
				case l: l;
				};
				var parts = Str.split_delim(Str.regexp("[;:]"), p);
				com.class_path = ::("", List.map(normalize_path, loop(parts)));
			} catch (e: Not_found) {
				if ( = (Sys.os_type, "Unix")) {
					com.class_path = ::("/usr/lib/haxe/std/", ::("/usr/share/haxe/std/", ::("/usr/local/lib/haxe/std/",
										::("/usr/lib/haxe/extraLibs/", ::("/usr/local/lib/haxe/extraLibs/", ::("", []))))));
				} else {
					var base_path = normalize_path(get_real_path(try {
						executable_path([]);
					} catch (e: _) {
						"./";
					}));
					com.class_path = ::( ^ (base_path, "std/"), ::( ^ (base_path, "extraLibs/"), ::("", [])));
				};
			};
			com.std_path = List.filter(function p: || (ExtString.String.ends_with(p, "std/"), ExtString.String.ends_with(p, "std\\")), com.class_path);
			function set_platform(pf, file) return {
				if (<>(com.platform, Cross)) {
					failwith("Multiple targets");
				} else {
					[];
				};
				Common.init_platform(com, pf);
				com.file = file;
				if ( && ( = (pf, Flash), = (file_extension(file), "swc"))) {
					Common.define(com, Define.Swc);
				} else {
					[];
				};
			};
			function define(f) return {
				Arg.Unit(function []: Common.define(com, f));
			};
			var process_ref = ref(function args: []);
			function process_libs([]) return {
				var libs = List.filter(function l: !(Hashtbl.mem(added_libs, l)), List.rev(cp_libs.val));
				cp_libs.val = [];
				List.iter(function l: Hashtbl.add(added_libs, l, []), libs);
				switch (add_libs(com, libs)) {
				case []: [];
				case args: process_ref.val(args);
				};
			};
			var arg_delays = ref([]);
			var basic_args_spec = ::((new Tuple("-cp", Arg.String(function path: process_libs([]);
												com.class_path = ::(normalize_path(path), com.class_path)), "<path> : add a directory to find source files")), ::((new Tuple("-js", Arg.String(set_platform(Js)), "<file> : compile code to JavaScript file")), ::((new Tuple("-swf", Arg.String(set_platform(Flash)), "<file> : compile code to Flash SWF file")), ::((new Tuple("-as3", Arg.String(function dir: set_platform(Flash, dir);
														Common.define(com, Define.As3);
														Common.define(com, Define.NoInline)), "<directory> : generate AS3 code into target directory")), ::((new Tuple("-neko", Arg.String(set_platform(Neko)), "<file> : compile code to Neko Binary")), ::((new Tuple("-php", Arg.String(function dir: classes.val = ::((new Tuple(::("php", []), "Boot")), classes.val);
																set_platform(Php, dir)), "<directory> : generate PHP code into target directory")), ::((new Tuple("-cpp", Arg.String(function dir: set_platform(Cpp, dir)), "<directory> : generate C++ code into target directory")), ::((new Tuple("-cs", Arg.String(function dir: cp_libs.val = ::("hxcs", cp_libs.val);
																		set_platform(Cs, dir)), "<directory> : generate C# code into target directory")), ::((new Tuple("-java", Arg.String(function dir: cp_libs.val = ::("hxjava", cp_libs.val);
																				set_platform(Java, dir)), "<directory> : generate Java code into target directory")), ::((new Tuple("-python", Arg.String(function dir: set_platform(Python, dir)), "<file> : generate Python code as target file")), ::((new Tuple("-xml", Arg.String(function file: Parser.use_doc.val = True;
																						xml_out.val = Some(file)), "<file> : generate XML types description")), ::((new Tuple("-main", Arg.String(function cl:
			if (<>(com.main_class, None)) {
			raise(Arg.Bad("Multiple -main"));
			} else {
				[];
			};
			var cpath = make_type_path(cl);
						com.main_class = Some(cpath);
						classes.val = ::(cpath, classes.val)), "<class> : select startup class")), ::((new Tuple("-lib", Arg.String(function l: cp_libs.val = ::(l, cp_libs.val);
								Common.raw_define(com, l)), "<library[:version]> : use a haxelib library")), ::((new Tuple("-D", Arg.String(function var:
			switch (var) {
		case no_copt | no-copt: com.foptimize = False;
		case use_rtti_doc | use-rtti-doc: Parser.use_doc.val = True;
		case _: if (List.mem(var, reserved_flags)) {
					raise(Arg.Bad( ^ (var, " is a reserved compiler flag and cannot be defined from command line")));
				} else {
					[];
				};
			};
			Common.raw_define(com, var)), "<var[=value]> : define a conditional compilation flag")), ::((new Tuple("-v", Arg.Unit(function []: com.verbose = True), ": turn on verbose mode")), ::((new Tuple("-debug", Arg.Unit(function []: Common.define(com, Define.Debug);
					com.debug = True), ": add debug information to the compiled code")), []))))))))))))))));
			var adv_args_spec = ::((new Tuple("-dce", Arg.String(function mode:
			switch (mode) {
		case std | full | no: [];
			case _: raise(Arg.Bad("Invalid DCE mode, expected std | full | no"));
			};
			Common.define_value(com, Define.Dce, mode)), "[std|full|no] : set the dead code elimination mode [default std]")), ::((new Tuple("-swf-version", Arg.Float(function v:
			if ( || (!(swf_version.val), < (com.flash_version, v))) {
			com.flash_version = v;
		} else {
			[];
			};
			swf_version.val = True), "<version> : change the SWF version")), ::((new Tuple("-swf-header", Arg.String(function h:
			try {
				swf_header.val = Some(switch (ExtString.String.nsplit(h, ":")) {
			case ::(width, ::(height, ::(fps, []))): (new Tuple(int_of_string(width), int_of_string(height), float_of_string(fps),
					0xFFFFFF));
				case ::(width, ::(height, ::(fps, ::(color, [])))): var color = if (ExtString.String.starts_with(color, "0x")) {
						color;
					} else {
						^ ("0x", color);
					};
					(new Tuple(int_of_string(width), int_of_string(height), float_of_string(fps), int_of_string(color)));
				case _: raise(Exit);
				});
			} catch (e: _) {
				raise(Arg.Bad("Invalid SWF header format, expected width:height:fps[:color]"));
			}), "<header> : define SWF header [width:height:fps:color]")), ::((new Tuple("-swf-lib", Arg.String(function file: process_libs([]);
					Genswf.add_swf_lib(com, file, False)), "<file> : add the SWF library to the compiled SWF")), ::((new Tuple("-swf-lib-extern", Arg.String(function file: Genswf.add_swf_lib(com, file, True)), "<file> : use the SWF library for type checking")), ::((new Tuple("-java-lib", Arg.String(function file: var std = = (file, "lib/hxjava-std.jar");
			arg_delays.val = ::(function []: Genjava.add_java_lib(com, file, std), arg_delays.val)), "<file> : add an external JAR or class directory library")), ::((new Tuple("-net-lib", Arg.String(function file: var Tuple(file, is_std) = switch (ExtString.String.nsplit(file, "@")) {
		case ::(file, []): (new Tuple(file, False));
			case ::(file, ::(std, [])): (new Tuple(file, True));
			case _: raise(Exit);
			};
			arg_delays.val = ::(function []: Gencs.add_net_lib(com, file, is_std), arg_delays.val)), "<file>[@std] : add an external .NET DLL file")), ::((new Tuple("-net-std", Arg.String(function file: Gencs.add_net_std(com, file)), "<file> : add a root std .NET DLL search path")), ::((new Tuple("-c-arg", Arg.String(function arg: com.c_args = ::(arg, com.c_args)), "<arg> : pass option <arg> to the native Java/C# compiler")), ::((new Tuple("-x", Arg.String(function file: var neko_file = ^ (file, ".n");
					set_platform(Neko, neko_file);
			if ( = (com.main_class, None)) {
			var cpath = make_type_path(file);
				com.main_class = Some(cpath);
				classes.val = ::(cpath, classes.val);
			} else {
				[];
			};
			cmds.val = ::( ^ ("neko ", neko_file), cmds.val)), "<file> : shortcut for compiling and executing a neko file")), ::((new Tuple("-resource", Arg.String(function res: var Tuple(file, name) = switch (ExtString.String.nsplit(res, "@")) {
		case ::(file, ::(name, [])): (new Tuple(file, name));
			case ::(file, []): (new Tuple(file, file));
			case _: raise(Arg.Bad("Invalid Resource format, expected file@name"));
			};
			var file = try {
				Common.find_file(com, file);
			} catch (e: Not_found) {
				file;
			};
			var data = try {
				var s = Std.input_file(bin = True, file);
				if ( > (String.length(s), 12000000)) {
					raise(Exit);
				} else {
					[];
				};
				s;
			} catch (e: T) {
				McOr(McArr(PaApp(PaId(IdUid(<...>)), PaAny), ExNil, ExApp(ExId(IdLid(<...>)), ExApp(ExApp(<...>, <...>), ExId(<...>)))),
					 McArr(PaAny, ExNil, ExApp(ExId(IdLid(<...>)), ExApp(ExApp(<...>, <...>), ExApp(<...>,
										   <...>)))))				case Sys_error(_): failwith( ^ ("Resource file not found : ", file));
			case _: failwith( ^ ("Resource '", ^ (file, "' excess the maximum size of 12MB")));
			};
			if (Hashtbl.mem(com.resources, name)) {
			failwith( ^ ("Duplicate resource name ", name));
			} else {
				[];
			};
			Hashtbl.add(com.resources, name, data)), "<file>[@name] : add a named resource file")), ::((new Tuple("-prompt", Arg.Unit(function []: prompt.val = True), ": prompt on error")), ::((new Tuple("-cmd", Arg.String(function cmd: cmds.val = ::(unquote(cmd), cmds.val)), ": run the specified command after successful compilation")), ::((new Tuple("--flash-strict", define(Define.FlashStrict), ": more type strict flash API")), ::((new Tuple("--no-traces", define(Define.NoTraces), ": don't compile trace calls in the program")), ::((new Tuple("--gen-hx-classes", Arg.Unit(function []: force_typing.val = True;
					pre_compilation.val = ::(function []: List.iter(function (_, _, extract): Hashtbl.iter(function n: function _: classes.val = ::(n, classes.val), extract([])), com.swf_libs);
											 List.iter(function (_, std, _, all_files, _):
			if (!(std)) {
			List.iter(function path:
			if (<>(path, (new Tuple(::("java", ::("lang", [])), "String")))) {
				classes.val = ::(path, classes.val);
				} else {
					[];
				}, all_files([]));
			} else {
				[];
			}, com.java_libs);
			List.iter(function (_, std, all_files, _):
			if (!(std)) {
			List.iter(function path: classes.val = ::(path, classes.val), all_files([]));
			} else {
				[];
			}, com.net_libs), pre_compilation.val);
			xml_out.val = Some("hx")), ": generate hx headers for all input classes")), ::((new Tuple("--next", Arg.Unit(function []: assert False), ": separate several haxe compilations")), ::((new Tuple("--each", Arg.Unit(function []: assert False), ": append preceding parameters to all haxe compilations separated by --next")), ::((new Tuple("--display", Arg.String(function file_pos:
			switch (file_pos) {
		case classes: pre_compilation.val = ::(function []: raise(Parser.TypePath(::(".", []), None, True)), pre_compilation.val);
			case keywords: complete_fields(com, Hashtbl.fold(function k: function _: function acc: ::((new Tuple(k, "", None, "")),
				acc), Lexer.keywords, []));
			case memory: did_something.val = True;
				try {
					display_memory(ctx);
				} catch (e: e) {
					prerr_endline(Printexc.get_backtrace([]));
				};
			case _: var Tuple(file, pos) = try {
					ExtString.String.split(file_pos, "@");
				} catch (e: _) {
					failwith( ^ ("Invalid format : ", file_pos));
				};
				var file = unquote(file);
				var Tuple(pos, smode) = try {
					ExtString.String.split(pos, "@");
				} catch (e: _) {
					(new Tuple(pos, ""));
				};
				function activate_special_display_mode([]) return {
					Common.define(com, Define.NoCOpt);
					Parser.use_parser_resume.val = False;
				};
				var mode = switch (smode) {
				case position: activate_special_display_mode([]);
					DMPosition;
				case usage: activate_special_display_mode([]);
					DMUsage;
				case type: activate_special_display_mode([]);
					DMType;
				case toplevel: activate_special_display_mode([]);
					DMToplevel;
				case : Parser.use_parser_resume.val = True;
					DMDefault;
				case _: var Tuple(smode, arg) = try {
						ExtString.String.split(smode, "@");
					} catch (e: _) {
						(new Tuple(pos, ""));
					};
					switch (smode) {
					case resolve: activate_special_display_mode([]);
						DMResolve(arg);
					case _: Parser.use_parser_resume.val = True;
						DMDefault;
					};
				};
				var pos = try {
					int_of_string(pos);
				} catch (e: _) {
					failwith( ^ ("Invalid format : ", pos));
				};
				com.display = mode;
				Common.display_default.val = mode;
				Common.define_value(com, Define.Display, if (<>(smode, "")) {
				smode;
			} else {
				"1";
			});
				Parser.use_doc.val = True;
				Parser.resume_display.val = { () with Ast.pfile = Common.unique_full_path(file);
											  Ast.pmin = pos;
											  Ast.pmax = pos
											};
			}), ": display code tips")), ::((new Tuple("--no-output", Arg.Unit(function []: no_output.val = True), ": compiles but does not generate any file")), ::((new Tuple("--times", Arg.Unit(function []: measure_times.val = True), ": measure compilation times")), ::((new Tuple("--no-inline", define(Define.NoInline), ": disable inlining")), ::((new Tuple("--no-opt", Arg.Unit(function []: com.foptimize = False;
											Common.define(com, Define.NoOpt)), ": disable code optimizations")), ::((new Tuple("--php-front", Arg.String(function f:
			if (<>(com.php_front, None)) {
			raise(Arg.Bad("Multiple --php-front"));
			} else {
				[];
			};
			com.php_front = Some(f)), "<filename> : select the name for the php front file")), ::((new Tuple("--php-lib", Arg.String(function f:
			if (<>(com.php_lib, None)) {
			raise(Arg.Bad("Multiple --php-lib"));
			} else {
				[];
			};
			com.php_lib = Some(f)), "<filename> : select the name for the php lib folder")), ::((new Tuple("--php-prefix", Arg.String(function f:
			if (<>(com.php_prefix, None)) {
			raise(Arg.Bad("Multiple --php-prefix"));
			} else {
				[];
			};
			com.php_prefix = Some(f);
			Common.define(com, Define.PhpPrefix)), "<name> : prefix all classes with given name")), ::((new Tuple("--remap", Arg.String(function s: var Tuple(pack, target) = try {
				ExtString.String.split(s, ":");
			} catch (e: _) {
				raise(Arg.Bad("Invalid remap format, expected source:target"));
			};
			com.package_rules = PMap.add(pack, Remap(target), com.package_rules)), "<package:target> : remap a package to another one")), ::((new Tuple("--interp", Arg.Unit(function []: Common.define(com, Define.Interp);
					set_platform(Neko, "");
					interp.val = True), ": interpret the program using internal macro system")), ::((new Tuple("--macro", Arg.String(function e: force_typing.val = True;
							config_macros.val = ::(e, config_macros.val)), " : call the given macro before typing anything else")), ::((new Tuple("--eval", Arg.String(function s: force_typing.val = True;
			evals.val = ::(s, evals.val)), " : evaluates argument as Haxe module code")), ::((new Tuple("--wait", Arg.String(function hp: var Tuple(host, port) = try {
				ExtString.String.split(hp, ":");
			} catch (e: _) {
				(new Tuple("127.0.0.1", hp));
			};
			wait_loop(com, host, try {
				int_of_string(port);
			} catch (e: _) {
				raise(Arg.Bad("Invalid port"));
			})), "<[host:]port> : wait on the given port for commands to run]")), ::((new Tuple("--connect", Arg.String(function _: assert False), "<[host:]port> : connect on the given port and run commands there]")), ::((new Tuple("--cwd", Arg.String(function dir: assert False), "<dir> : set current working directory")), ::((new Tuple("-version", Arg.Unit(function []: message(ctx, s_version, Ast.null_pos);
			did_something.val = True), ": print version and exit")), ::((new Tuple("--help-defines", Arg.Unit(function []: var m = ref(0);
			function loop(i) return {
				var d = Obj.magic(i);
				if (<>(d, Define.Last)) {
					var Tuple(t, doc) = Define.infos(d);
					if ( > (String.length(t), m.val)) {
						m.val = String.length(t);
					} else {
						[];
					};
					::((new Tuple(String.concat("-", ExtString.String.nsplit(t, "_")), doc)), loop(+(i, 1)));
				} else {
					[];
				};
			};
			var all = List.sort(function (s1, _): function (s2, _): String.compare(s1, s2), loop(0));
					  var all = List.map(function (n, doc): Printf.sprintf(" %-*s: %s", m.val, n, limit_string(doc, +(m.val, 3))), all);
					  List.iter(function msg: ctx.com.print( ^ (msg, "\n")), all);
					  did_something.val = True), ": print help for all compiler specific defines")), ::((new Tuple("--help-metas", Arg.Unit(function []: var m = ref(0);
			function loop(i) return {
				var d = Obj.magic(i);
				if (<>(d, Meta.Last)) {
					var Tuple(t, Tuple(doc, flags)) = MetaInfo.to_string(d);
					if (!(List.mem(MetaInfo.Internal, flags))) {
						var params = ref([]);
						var used = ref([]);
						var pfs = ref([]);
					List.iter(function case MetaInfo.HasParam(s): params.val = ::(s, params.val);
						case MetaInfo.Platform(f): pfs.val = ::(f, pfs.val);
							case MetaInfo.Platforms(fl): pfs.val = @(fl, pfs.val);
								case MetaInfo.UsedOn(u): used.val = ::(u, used.val);
									case MetaInfo.UsedOnEither(ul): used.val = @(ul, used.val);
										case MetaInfo.Internal: assert False, flags);
						var params = switch (List.rev(params.val)) {
						case []: "";
						case l: ^ ("[", ^ (String.concat(",", l), "]"));
						};
						var pfs = switch (List.rev(pfs.val)) {
						case []: "";
						case ::(p, []): ^ (", [", ^ (platform_name(p), " only]"));
						case pl: ^ (", [for ", ^ (String.concat(",", List.map(platform_name, pl)), "]"));
						};
						var str = ^ ("@", t);
						if ( > (String.length(str), m.val)) {
							m.val = String.length(str);
						} else {
							[];
						};
						::((new Tuple(str, ^ (params, ^ (doc, pfs)))), loop(+(i, 1)));
					} else {
						loop(+(i, 1));
					};
				} else {
					[];
				};
			};
			var all = List.sort(function (s1, _): function (s2, _): String.compare(s1, s2), loop(0));
					  var all = List.map(function (n, doc): Printf.sprintf(" %-*s: %s", m.val, n, limit_string(doc, +(m.val, 3))), all);
					  List.iter(function msg: ctx.com.print( ^ (msg, "\n")), all);
					  did_something.val = True), ": print help for all compiler metadatas")), []))))))))))))))))))))))))))))))))))));
			function args_callback(cl) return {
				var Tuple(path, name) = make_path(cl);
				if (starts_uppercase(name)) {
					classes.val = ::((new Tuple(path, name)), classes.val);
				} else {
					force_typing.val = True;
					config_macros.val = ::(Printf.sprintf("include['%s']", cl), config_macros.val);
				};
			};
			var all_args_spec = @(basic_args_spec, adv_args_spec);
			function process(args) return {
				var current = ref(0);
				try {
					Arg.parse_argv(current = , Array.of_list(::("", List.map(expand_env, args))), all_args_spec, args_callback, usage);
					List.iter(function fn: fn([]), arg_delays.val);
				} catch (e: Arg.Bad(msg) = exc) {
					var r = Str.regexp("unknown option `\\[[-A-Za-z]+\\]'");
					try {
						ignore(Str.search_forward(r, msg, 0));
						var s = Str.matched_group(1, msg);
						var sl = List.map(function (s, _, _): s, all_args_spec);
						var msg = Typecore.string_error_raise(s, sl, Printf.sprintf("Invalid command: %s", s));
						raise(Arg.Bad(msg));
					} catch (e: Not_found) {
						raise(exc);
					};
				};
				arg_delays.val = [];
			};
			process_ref.val = process;
			process(ctx.com.args);
			process_libs([]);
			if (<>(com.display, DMNone)) {
				com.warning = message(ctx);
				com.error = error(ctx);
				com.main_class = None;
				var real = get_real_path(Parser.resume_display.val.Ast.pfile);
				classes.val = lookup_classes(com, real);
				if ( = (classes.val, [])) {
					if (!(Sys.file_exists(real))) {
						failwith("Display file does not exist");
					} else {
						[];
					};
					switch (List.rev(ExtString.String.nsplit(real, path_sep))) {
					case ::(file, _) if (&&(>=(file0, 'a'), <=(file1, 'z'))): failwith( ^ ("Display file '", ^ (file,
								"' should not start with a lowercase letter")));
					case _: [];
					};
					failwith("Display file was not found in class path");
				} else {
					[];
				};
				Common.log(com, ^ ("Display file : ", real));
				Common.log(com, ^ ("Classes found : [", ^ (String.concat(",", List.map(Ast.s_type_path, classes.val)), "]")));
			} else {
				[];
			};
			function add_std(dir) return {
				com.class_path = @(List.filter(function s: !(List.mem(s, com.std_path)), com.class_path), @(List.map(function p: ^ (p, ^ (dir, "/_std/")), com.std_path), com.std_path));
			};
			var ext = switch (com.platform) {
			case Cross: set_platform(Cross, "");
				"?";
			case Flash: function loop(match) return switch (match) {
				case []: [];
				case ::((v, _), _) if (>(v, com.flash_version)): [];
				case ::((v, def), l): Common.raw_define(com, ^ ("flash", def));
					loop(l);
				};
				loop(Common.flash_versions);
				Common.raw_define(com, "flash");
				com.package_rules = PMap.remove("flash", com.package_rules);
				add_std("flash");
				"swf";
			case Neko: add_std("neko");
				"n";
			case Js: if (!(PMap.exists(fst(Define.infos(Define.JqueryVer)), com.defines))) {
					Common.define_value(com, Define.JqueryVer, "11103");
				} else {
					[];
				};
				add_std("js");
				"js";
			case Php: add_std("php");
				"php";
			case Cpp: add_std("cpp");
				"cpp";
			case Cs: var old_flush = ctx.flush;
				ctx.flush = function []: com.net_libs = [];
				old_flush([]);
				Gencs.before_generate(com);
				add_std("cs");
				"cs";
			case Java: var old_flush = ctx.flush;
				ctx.flush = function []: List.iter(function (_, _, close, _, _): close([]), com.java_libs);
				com.java_libs = [];
				old_flush([]);
				Genjava.before_generate(com);
				add_std("java");
				"java";
			case Python: add_std("python");
				"python";
			};
			switch (com.display) {
			case DMNone | DMToplevel: [];
			case _: if (!(ctx.has_next)) {
					com.package_rules = PMap.foldi(function p: function r: function acc:
					switch (r) {
				case Forbidden: acc;
				case _: PMap.add(p, r, acc);
					}, com.package_rules, PMap.empty);
				} else {
					[];
				};
			};
			com.config = get_config(com);
			List.iter(function f: f([]), List.rev(pre_compilation.val));
			if ( && ( = (classes.val, ::((new Tuple([], "Std")), [])), !(force_typing.val))) {
				var help_spec = @(basic_args_spec, ::((new Tuple("-help", Arg.Unit(function []: []), ": show extended help information")),
													  ::((new Tuple("--help", Arg.Unit(function []: []), ": show extended help information")), ::((new Tuple("--help-defines",
															  Arg.Unit(function []: []), ": print help for all compiler specific defines")), ::((new Tuple("--help-metas",
																	  Arg.Unit(function []: []), ": print help for all compiler metadatas")), ::((new Tuple("<dot-path>",
																			  Arg.Unit(function []: []), ": compile the module specified by dot-path")), []))))));
				if ( && ( = (cmds.val, []), !(did_something.val))) {
					Arg.usage(help_spec, usage);
				} else {
					[];
				};
			} else {
				ctx.setup([]);
				Common.log(com, ^ ("Classpath : ", String.concat(";", com.class_path)));
				Common.log(com, ^ ("Defines : ", String.concat(";", PMap.foldi(function k: function v: function acc: ::(switch (v) {
			case 1: k;
			case _: ^ (k, ^ ("=", v));
				}, acc), com.defines, []))));
				var t = Common.timer("typing");
				Typecore.type_expr_ref.val = function ctx: function e: function with_type: Typer.type_expr(ctx, e, with_type);
				var tctx = Typer.create(com);
				List.iter(Typer.call_init_macro(tctx), List.rev(config_macros.val));
				List.iter(Typer.eval(tctx), evals.val);
				List.iter(function cpath: ignore(tctx.Typecore.g.Typecore.do_load_module(tctx, cpath, Ast.null_pos)), List.rev(classes.val));
				Typer.finalize(tctx);
				t([]);
				if (ctx.has_error) {
					raise(Abort);
				} else {
					[];
				};
				switch (com.display) {
				case DMNone | DMUsage | DMPosition | DMType | DMResolve(_): [];
				case _: if (ctx.has_next) {
						raise(Abort);
					} else {
						[];
					};
					failwith("No completion point was found");
				};
				var t = Common.timer("filters");
				var Tuple(main, types, modules) = Typer.generate(tctx);
				com.main = main;
				com.types = types;
				com.modules = modules;
				Filters.run(com, tctx, main);
				if (ctx.has_error) {
					raise(Abort);
				} else {
					[];
				};
				if ( && (!(no_output.val), = (file_extension(com.file), ext))) {
					delete_file(com.file);
				} else {
					[];
				};
				switch (xml_out.val) {
				case None: [];
				case Some(hx): Genxml.generate_hx(com);
				case Some(file): Common.log(com, ^ ("Generating xml : ", file));
					Common.mkdir_from_path(file);
					Genxml.generate(com, file);
				};
				if ( || ( = (com.platform, Flash), = (com.platform, Cpp))) {
					List.iter(Codegen.fix_overrides(com), com.types);
				} else {
					[];
				};
				if (Common.defined(com, Define.Dump)) {
					Codegen.dump_types(com);
				} else {
					[];
				};
				if (Common.defined(com, Define.DumpDependencies)) {
					Codegen.dump_dependencies(com);
				} else {
					[];
				};
				t([]);
				if (!(no_output.val)) {
					switch (com.platform) {
					case Neko if (interp.val): [];
					case Cpp if (Common.defined(com, Define.Cppia)): [];
					case Cpp | Cs | Java | Php: Common.mkdir_from_path( ^ (com.file, "/."));
					case _: Common.mkdir_from_path(com.file);
					};
				} else {
					[];
				};
				if (!(no_output.val)) {
					if (interp.val) {
						var ctx = Interp.create(com, Typer.make_macro_api(tctx, Ast.null_pos));
						Interp.add_types(ctx, com.types, function t: []);
						switch (com.main) {
						case None: [];
						case Some(e): ignore(Interp.eval_expr(ctx, e));
						};
					} else {
						if ( = (com.platform, Cross)) {
							[];
						} else {
							var Tuple(generate, name) = switch (com.platform) {
							case Flash if (Common.defined(com, Define.As3)): (new Tuple(Genas3.generate, "AS3"));
							case Flash: (new Tuple(Genswf.generate(swf_header.val), "swf"));
							case Neko: (new Tuple(Genneko.generate, "neko"));
							case Js: (new Tuple(Genjs.generate, "js"));
							case Php: (new Tuple(Genphp.generate, "php"));
							case Cpp: (new Tuple(Gencpp.generate, "cpp"));
							case Cs: (new Tuple(Gencs.generate, "cs"));
							case Java: (new Tuple(Genjava.generate, "java"));
							case Python: (new Tuple(Genpy.generate, "python"));
							case Cross: assert False;
							};
							Common.log(com, ^ ("Generating ", ^ (name, ^ (": ", com.file))));
							var t = Common.timer( ^ ("generate ", name));
							generate(com);
							t([]);
						};
					};
				} else {
					[];
				};
			};
			Sys.catch_break(False);
			List.iter(function f: f([]), List.rev(com.final_filters));
			if (!(no_output.val)) {
				List.iter(function c: var r = run_command(ctx, c);
				if (<>(r, 0)) {
				failwith( ^ ("Command failed with error ", string_of_int(r)));
				} else {
					[];
				}, List.rev(cmds.val));
			} else {
				[];
			};
		} catch (e: T) {
			McOr(McArr(PaId(IdUid(Abort)), ExNil, ExId(IdUid([]))), McOr(McArr(PaApp(PaApp(<...>, <...>), PaId(<...>)), ExNil,
					ExApp(ExApp(<...>, <...>), ExId(<...>))), McOr(McArr(PaApp(<...>, <...>), ExNil, ExApp(<...>, <...>)), McOr(McArr(<...>,
						<...>, <...>), McOr(<...>, <...>)))))			case Abort: [];
		case Ast.Error(m, p): error(ctx, m, p);
		case Typecore.Fatal_error(m, p): error(ctx, m, p);
		case Common.Abort(m, p): error(ctx, m, p);
		case Lexer.Error(m, p): error(ctx, Lexer.error_msg(m), p);
		case Parser.Error(m, p): error(ctx, Parser.error_msg(m), p);
		case Typecore.Forbid_package((pack, m, p), pl, pf): if ( && (<>(Common.display_default.val, DMNone), ctx.has_next)) {
				ctx.has_error = False;
				ctx.messages = [];
			} else {
				error(ctx, Printf.sprintf("You cannot access the %s package while %s [for %s]", pack, if ( = (pf, "macro")) {
				"in a macro";
			} else {
				^ ("targeting ", pf);
				}, Ast.s_type_path(m)), p);
				List.iter(error(ctx, "    referenced here"), List.rev(pl));
			};
		case Typecore.Error(m, p): error(ctx, Typecore.error_msg(m), p);
		case Interp.Error(msg, ::(p, l)): message(ctx, msg, p);
			List.iter(message(ctx, "Called from"), l);
			error(ctx, "Aborted", Ast.null_pos);
		case Codegen.Generic_Exception(m, p): error(ctx, m, p);
		case Arg.Bad(msg): error(ctx, ^ ("Error: ", msg), Ast.null_pos);
		case Failure(msg) if (!(is_debug_run([]))): error(ctx, ^ ("Error: ", msg), Ast.null_pos);
		case Arg.Help(msg): message(ctx, msg, Ast.null_pos);
		case Typer.DisplayFields(fields): var ctx = print_context([]);
			var fields = List.map(function (name, t, kind, doc): (new Tuple(name, s_type(ctx, t), kind, switch (doc) {
		case None: "";
		case Some(d): d;
			})), fields);
			var fields = if (measure_times.val) {
				close_times([]);
				var tot = ref(0.);
				Hashtbl.iter(function _: function t: tot.val = +.(tot.val, t.total), Common.htimers);
				var fields = ::((new Tuple("@TOTAL", Printf.sprintf("%.3fs", -.(get_time([]), start_time.val)), None, "")), fields);
				if ( > (tot.val, 0.)) {
					Hashtbl.fold(function _: function t: function acc: ::((new Tuple( ^ ("@TIME ", t.name), Printf.sprintf("%.3fs [%.0f%%]",
								 t.total, / .( * .(t.total, 100.), tot.val)), None, "")), acc), Common.htimers, fields);
				} else {
					fields;
				};
			} else {
				fields;
			};
			complete_fields(com, fields);
		case Typecore.DisplayTypes(tl): var ctx = print_context([]);
			var b = Buffer.create(0);
			List.iter(function t: Buffer.add_string(b, "<type>\n");
					  Buffer.add_string(b, htmlescape(s_type(ctx, t)));
					  Buffer.add_string(b, "\n</type>\n"), tl);
			raise(Completion(Buffer.contents(b)));
		case Typecore.DisplayPosition(pl): var b = Buffer.create(0);
			function error_printer(file, line) return {
				sprintf("%s:%d:", Common.unique_full_path(file), line);
			};
			Buffer.add_string(b, "<list>\n");
			List.iter(function p: var epos = Lexer.get_error_pos(error_printer, p);
					  Buffer.add_string(b, "<pos>");
					  Buffer.add_string(b, epos);
					  Buffer.add_string(b, "</pos>\n"), pl);
			Buffer.add_string(b, "</list>");
			raise(Completion(Buffer.contents(b)));
		case Typer.DisplayToplevel(il): var b = Buffer.create(0);
			Buffer.add_string(b, "<il>\n");
			var ctx = print_context([]);
			function s_type(t) return {
				htmlescape(s_type(ctx, t));
			};
			List.iter(function id:
			switch (id) {
		case Typer.ITLocal(v): Buffer.add_string(b, Printf.sprintf("<i k=\"local\" t=\"%s\">%s</i>\n", s_type(v.v_type),
						v.v_name));
			case Typer.ITMember(c, cf): Buffer.add_string(b, Printf.sprintf("<i k=\"member\" t=\"%s\">%s</i>\n", s_type(cf.cf_type),
						cf.cf_name));
			case Typer.ITStatic(c, cf): Buffer.add_string(b, Printf.sprintf("<i k=\"static\" t=\"%s\">%s</i>\n", s_type(cf.cf_type),
						cf.cf_name));
			case Typer.ITEnum(en, ef): Buffer.add_string(b, Printf.sprintf("<i k=\"enum\" t=\"%s\">%s</i>\n", s_type(ef.ef_type),
						ef.ef_name));
			case Typer.ITGlobal(mt, s, t): Buffer.add_string(b, Printf.sprintf("<i k=\"global\" p=\"%s\" t=\"%s\">%s</i>\n",
						s_type_path(t_infos(mt).mt_path), s_type(t), s));
			case Typer.ITType(mt): Buffer.add_string(b, Printf.sprintf("<i k=\"type\" p=\"%s\">%s</i>\n",
						s_type_path(t_infos(mt).mt_path), snd(t_infos(mt).mt_path)));
			case Typer.ITPackage(s): Buffer.add_string(b, Printf.sprintf("<i k=\"package\">%s</i>\n", s));
			}, il);
			Buffer.add_string(b, "</il>");
			raise(Completion(Buffer.contents(b)));
		case Parser.TypePath(p, c, is_import): switch (c) {
			case None: var Tuple(packs, classes) = read_type_path(com, p);
				if ( && ( = (packs, []), = (classes, []))) {
					error(ctx, ^ ("No classes found in ", String.concat(".", p)), Ast.null_pos);
				} else {
					complete_fields(com, 	function convert(k, f) return {
						(new Tuple(f, "", Some(k), ""));
					};
					@(List.map(convert(Typer.FKPackage), packs), List.map(convert(Typer.FKType), classes)));
				};
			case Some(c, cur_package): try {
					var Tuple(sl_pack, s_module) = switch (List.rev(p)) {
					case ::(s, sl) if (&&(>=(s0, 'A'), <=(s0, 'Z'))): (new Tuple(List.rev(sl), s));
					case _: (new Tuple(p, c));
					};
					var ctx = Typer.create(com);
					function lookup(p) return {
						try {
							Typeload.load_module(ctx, (new Tuple(p, s_module)), Ast.null_pos);
						} catch (e: e) {
							if (cur_package) {
								switch (List.rev(p)) {
								case []: raise(e);
								case ::(_, p): lookup(List.rev(p));
								};
							} else {
								raise(e);
							};
						};
					};
					var m = lookup(sl_pack);
					var statics = ref(None);
					var public_types = List.filter(function t: var tinfos = t_infos(t);
												   var is_module_type = = (snd(tinfos.mt_path), c);
					if ( && (is_import, is_module_type)) {
					switch (t) {
						case TClassDecl(c): ignore(c.cl_build([]));
							statics.val = Some(c.cl_ordered_statics);
						case _: [];
						};
					} else {
						[];
					};
					!(tinfos.mt_private), m.m_types);
					var types = if (<>(c, s_module)) {
						[];
					} else {
						List.map(function t: (new Tuple(snd(t_path(t)), "", Some(Typer.FKType), "")), public_types);
					};
					var ctx = print_context([]);
					function make_field_doc(cf) return {
						(new Tuple(cf.cf_name, s_type(ctx, cf.cf_type), Some(switch (cf.cf_kind) {
					case Method(_): Typer.FKMethod;
						case Var(_): Typer.FKVar;
						}), switch (cf.cf_doc) {
					case Some(s): s;
						case None: "";
						}));
					};
					var types = switch (statics.val) {
					case None: types;
					case Some(cfl): @(types, List.map(make_field_doc, List.filter(function cf: cf.cf_public, cfl)));
					};
					complete_fields(com, types);
				} catch (e: T) {
					McOr(McArr(PaApp(PaId(IdUid(<...>)), PaId(IdLid(<...>))), ExNil, ExApp(ExId(IdLid(<...>)), ExApp(ExId(<...>),
							   ExId(<...>)))), McArr(PaAny, ExNil, ExApp(ExApp(ExApp(<...>, <...>), ExApp(<...>, <...>)), ExId(IdAcc(<...>,
												 <...>)))))																								case Completion(c): raise(Completion(c));
				case _: error(ctx, ^ ("Could not load module ", Ast.s_type_path((new Tuple(p, c)))), Ast.null_pos);
				};
			};
		case Interp.Sys_exit(i): ctx.flush([]);
			exit(i);
		case e if (&&(try {
					|| (<>(Sys.getenv("OCAMLRUNPARAM"), "b"), <>(global_cache.val, None));
					} catch (e: _) {
						True;
					}, !(is_debug_run([])))): error(ctx, Printexc.to_string(e), Ast.null_pos);
		};
	};

	public static function __init__() {
		var other = Common.timer("other");
		Sys.catch_break(True);
		var args = List.tl(Array.to_list(Sys.argv));
		try {
			var server = Sys.getenv("HAXE_COMPILATION_SERVER");
			var Tuple(host, port) = try {
				ExtString.String.split(server, ":");
			} catch (e: _) {
				(new Tuple("127.0.0.1", server));
			};
			do_connect(host, try {
				int_of_string(port);
			} catch (e: _) {
				failwith("Invalid HAXE_COMPILATION_SERVER port");
			}, args);
		} catch (e: Not_found) {
			try {
				process_params(create_context, args);
			} catch (e: Completion(c)) {
				prerr_endline(c);
				exit(0);
			};
		};
		other([]);
		if (measure_times.val) {
			report_times(prerr_endline);
		} else {
			[];
		};
	}
}
;
