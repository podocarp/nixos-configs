local luasnip = require("luasnip")
local s = luasnip.snippet
local i = luasnip.insert_node
local t = luasnip.text_node
local f = luasnip.function_node

return {
	s("multirow", { t("\\multirow{"), i(1, "2"), t("}{*}{"), i(2, "text"), t("}"), i(3) }),
	s("multicol", { t("\\multicolumn{"), i(1, "2"), t("}{c}{"), i(2, "text"), t("}"), i(3) }),
	s("f", { t("\\frac{"), i(1, "n"), t("}{"), i(2, "d"), t("}"), i(3) }),
	s("pdv", { t("\\pdv{"), i(1, "n"), t("}{"), i(2, "d"), t("}"), i(3) }),
	s("pdvv", { t("\\pdv{"), i(1, "n"), t("}"), i(2) }),
	s("dv", { t("\\dv{"), i(1, "n"), t("}{"), i(2, "d"), t("}"), i(3) }),
	s("dvv", { t("\\dv{"), i(1, "n"), t("}"), i(2) }),
	s("dv2", { t("\\dv[2]{"), i(1, "n"), t("}{"), i(2, "d"), t("}"), i(3) }),
	s("ooo", { t("\\infty") }),
	s("int", { t("\\int{"), i(1, "n=1"), t("}^{"), i(2, "\\infty"), t("}"), i(0) }),
	s("sum", { t("\\sum_{"), i(1, "n=1"), t("}^{"), i(2, "\\infty"), t("}"), i(0) }),
	s("prod", { t("\\prod_{"), i(1, "n=1"), t("}^{"), i(2, "\\infty"), t("}"), i(0) }),
	s("lim", { t("\\lim_{"), i(1, "n"), t(" \\to "), i(2, "\\infty"), t("}"), i(0) }),

	s("beg", { t({ "\\begin{" }), i(1), t({ "}", "" }), i(2), t({ "", "\\end{" }), i(1), t("}") }),
	s("eg", { t({ "\\begin{example}", "" }), i(1), t({ "", "\\end{example}" }) }),
	s("aln", { t({ "\\begin{align}", "" }), i(1), t({ "", "\\end{align}" }) }),
	s("gat", { t({ "\\begin{gather*}", "" }), i(1), t({ "", "\\end{gather*}" }) }),
	s("alnn", { t({ "\\begin{align*}", "" }), i(1), t({ "", "\\end{align*}" }) }),
	s("eqn", { t({ "\\begin{equation}", "" }), i(1), t({ "", "\\end{equation}" }) }),
	s("eqnn", { t({ "\\begin{equation*}", "" }), i(1), t({ "", "\\end{equation*}" }) }),
	s("def", { t({ "\\begin{definition}[", "" }), i(1), t({ "]", "" }), i(2), t({ "", "\\end{definition}" }) }),
	s("thm", { t({ "\\begin{theorem}[", "" }), i(1), t({ "]", "" }), i(2), t({ "", "\\end{theorem}" }) }),
	s("thmm", { t({ "\\begin{theorem*}[", "" }), i(1), t({ "]", "" }), i(2), t({ "", "\\end{theorem*}" }) }),
	s("lem", { t({ "\\begin{lemma}[", "" }), i(1), t({ "]", "" }), i(2), t({ "", "\\end{lemma}" }) }),
	s("cor", { t({ "\\begin{corollary}[", "" }), i(1), t({ "]", "" }), i(2), t({ "", "\\end{corollary}" }) }),
	s("prop", { t({ "\\begin{proposition}[", "" }), i(1), t({ "]", "" }), i(2), t({ "", "\\end{proposition}" }) }),
	s("ex", { t({ "\\begin{exercise}[", "" }), i(1), t({ "]", "" }), i(2), t({ "", "\\end{exercise}" }) }),
	s("sol", { t({ "\\begin{solution}[", "" }), i(1), t({ "]", "" }), i(2), t({ "", "\\end{solution}" }) }),
	s("pf", { t({ "\\begin{proof}", "" }), i(1), t({ "", "\\end{proof}" }) }),

	s("no", { t({ "\\note{", "" }), i(1), t({ "", "}" }) }),
	s("mat", { t({ "\\begin{" }), i(1, "pbvVBsmall"), t({ "matrix}", "" }), i(2), t({ "", "\\end{" }), i(1), t(
		"matrix}") }),
	s("arr", { t({ "\\begin{array}{", "" }), i(1, "ccc"), t({ "}", "" }), i(2), t({ "", "\\end{array}" }) }),

	s("em", { t("\\emph{"), i(1), t("}"), i(2) }),
	s("abs", { t("\\abs{"), i(1), t("}"), i(2) }),
	s("norm", { t("\\norm{"), i(1), t("}"), i(2) }),
	s("<>", { t("\\langle "), i(1), t(" \\rangle "), i(2) }),
	s("dd", { t("\\dd{"), i(1), t("}"), i(2) }),

	s({
		trig = "mint(%S+)?",
		regTrig = true,
		hidden = false,
	}, {
		t("\\begin{minted}[fontsize=\\"), i(2, "footnotesize"), t("]{"),
		f(function(_, snip)
			local match = snip.captures[1]
			return match and match or "lang"
		end, {}),
		t("}\n"), i(3, "remember to fragile"), t("\n\\end{minted}")
	}),
	-- Inline minted code
	s("mintin", {
		t("\\mintinline{"), i(1, "lang"), t("}|"), i(2), t("|"), i(3)
	}),

	-- Frame environment for Beamer
	s("frame", {
		t("\\begin{frame}["), i(1, "t"), t("]\n\\frametitle{"), i(2, "title"), t("}\n\\framesubtitle{"),
		i(3, "subtitle"), t("}\n"), i(4), t("\n\\end{frame}")
	}),

	-- Algorithm block
	s("algo", {
		t("\\begin{algorithm}\n\\SetKwFunction{"), i(1, "func"), t("}{"), i(2, "name"), t("}\n\\Fn{\\"),
		i(1, "func"), t("($x$)}{\n"), i(3), t("}\n\\caption{"), i(4, "caption"), t("}\\label{alg:"), i(5, "label"),
		t("}\n\\end{algorithm}")
	}),

	-- Figure environment
	s("fig", {
		t("\\begin{figure}["), i(1, "htpb"), t("]\n\\centering\n\\includegraphics[width=0.8\\textwidth]{"),
		i(2, "image.png"), t("}\n\\caption{"), i(3, "Caption"), t("}\n\\label{fig:"), i(4, "fig-label"), t(
	"}\n\\end{figure}")
	}),

	-- Figure with subfigures
	s("subfigure", {
		t("\\begin{figure}[H]\n\\centering\n\\begin{subfigure}["), i(1, "t"), t("]{"), i(2, "0.49\\textwidth"),
		t("}\n\\centering\n\\includegraphics[width=\\linewidth]{"), i(3, "image.png"), t("}\n\\caption{"),
		i(4, "caption"), t("}\n\\end{subfigure}%\n\\hspace{0.01\\linewidth}%\n\\caption{"), i(5, "figure caption"),
		t("}\n\\label{fig:"), i(6, "fig-label"), t("}\n\\end{figure}")
	}),

	-- Subfigure block
	s("subfig", {
		t("\\begin{subfigure}["), i(1, "t"), t("]{"), i(2, "0.45\\textwidth"),
		t("}\n\\centering\n\\includegraphics[width=\\linewidth]{"), i(3, "image.png"), t("}\n\\caption{"),
		i(4, "caption"), t("}\n\\label{fig:"), i(5, "fig-label"), t("}\n\\end{subfigure}")
	}),

	-- PGFPlots environment
	s("plot", {
		t("\\begin{figure}["), i(1, "htpb"), t("]\n\\begin{center}\n\\begin{tikzpicture}\n\\begin{axis}["),
		t("axis lines=middle,\nxlabel=$x$,\nylabel=$y$\n]\n\\addplot [domain=-pi:pi, samples=100, color=blue] {"),
		i(2, "sin(deg(x))"), t(
	"};\n\\addlegendentry{$f(x)$}\n\\end{axis}\n\\end{tikzpicture}\n\\end{center}\n\\end{figure}")
	}),

	-- General TikZ Environment
	s("tikz", {
		t("\\begin{figure}["), i(1, "htpb"), t("]\n\\centering\n\\begin{tikzpicture}[node distance=2cm, auto]\n"),
		i(2), t("\n\\end{tikzpicture}\n\\end{figure}")
	}),

	-- TikZ Node
	s("node", {
		t("\\node["), i(1, "state"), t("] ("), i(2, "name"), t(") "), i(3, "at (0,0)"), t(" {$"), i(2, "name"), t(
	"$};")
	}),

	-- TikZCD (TikZ Commutative Diagram)
	s("tikzcd", {
		t("\\begin{equation*}\n\\begin{tikzcd}\n"), i(1), t("\n\\end{tikzcd}\n\\end{equation*}")
	}),
}
