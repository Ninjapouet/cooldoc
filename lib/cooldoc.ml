

module HTML (G : Odoc_html.Html_generator) = struct

  let bs = Odoc_html.bs

  let _ = Odoc_html.charset := "utf8"

  class html =
    object(self)
      inherit G.html as super

      method! meta b =
        super#meta b;
        bs b {|<script src="https://polyfill.io/v3/polyfill.min.js?features=es6"></script>
|};
        bs b {|<script id="MathJax-script" async src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"></script>
|}

      method! html_of_custom_text b start text =
        match start with
        | "math" -> bs b "\\("; self#html_of_text b text; bs b "\\)"
        | _ -> super#html_of_custom_text b start text

    end
end


let _ = try Odoc_args.extend_html_generator (module HTML) with
  | Failure _ -> ();;

module LaTeX (G : Odoc_latex.Latex_generator) = struct

  let pr = Format.printf
  let pf = Format.fprintf

  class latex =
    object(self)
      inherit G.latex as super

      method! latex_of_custom_text ppf start text =
        match start with
        | "math" ->
          pf ppf "\\(";
          self#latex_of_text ppf text;
          pf ppf "\\)"
        | _ ->
          super#latex_of_custom_text ppf start text

      method private latex_document_class ppf =
        pf ppf "\\documentclass[11pt]{article}@\n";

      method! latex_of_Raw ppf s = pr "%s@." s; pf ppf "%s" s

      method private latex_packages ppf =
        pf ppf "\\usepackage[utf8]{inputenc}@\n";
        pf ppf "\\usepackage[T1]{fontenc}@\n";
        pf ppf "\\usepackage{textcomp}@\n";
        pf ppf "\\usepackage{fullpage}@\n";
        pf ppf "\\usepackage{url}@\n";
        pf ppf "\\usepackage{ocamldoc}@\n"

      method! latex_header ppf module_list =
        self#latex_document_class ppf;
        self#latex_packages ppf;
        begin match !Odoc_info.Global.title with
          | None -> ()
          | Some s ->
            pf ppf "\\title{%s}@\n" @@ self#escape s;
        end;
        pf ppf "\\begin{document}@\n";
        begin match !Odoc_info.Global.title with
          | None -> ()
          | Some _ -> pf ppf "\\maketitle@\n"
        end;
        if !Odoc_info.Global.with_toc then pf ppf "\\tableofcontents@\n";
        let info = Odoc_info.apply_opt
            (Odoc_info.info_of_comment_file module_list)
            !Odoc_info.Global.intro_file in
        (match info with None -> () | Some _ -> pf ppf "\\vspace{0.2cm}@\n");
        self#latex_of_info ppf info;
        (match info with None -> () | Some _ -> pf ppf "@\n@\n")

    end

end

let _ = try Odoc_args.extend_latex_generator (module LaTeX) with
  | Failure _ -> ();;
