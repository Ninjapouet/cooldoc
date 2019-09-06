

let bs = Odoc_html.bs

module Generator (G : Odoc_html.Html_generator) = struct

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


let _ = Odoc_args.extend_html_generator (module Generator);;
