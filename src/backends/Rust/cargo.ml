(********************************************************************)
(*                                                                  *)
(*  The LustreC compiler toolset   /  The LustreC Development Team  *)
(*  Copyright 2012 -    --   ONERA - CNRS - INPT                    *)
(*                                                                  *)
(*  LustreC is free software, distributed WITHOUT ANY WARRANTY      *)
(*  under the terms of the GNU Lesser General Public License        *)
(*  version 2.1.                                                    *)
(*                                                                  *)
(********************************************************************)

open Format
open LustreSpec
open Corelang
open Utils

module type MODIFIERS_MKF =
sig (* dep was (bool * ident * top_decl list) *)
  val other_targets: Format.formatter -> string -> string -> dep_t list -> unit
end

module EmptyMod =
(struct
  let other_targets _ _ _ _ = ()
end: MODIFIERS_MKF)


module Main = functor (Mod: MODIFIERS_MKF) ->
              struct

  let print_cargo is_oracle name dir =

  let rsc_dir = "rsc" in
  let build_file = Format.sprintf "%s/build.rs" rsc_dir in

  (* Generate cargo configuration file. *)
  let out_channel = Format.sprintf "%s/Cargo.toml" dir |> open_out in
  let fmt = Format.formatter_of_out_channel out_channel in
  Format.fprintf fmt
    "\
      [package]@.\
      name = \"%s_%s\"@.\
      version = \"1.0.0\"@.\
      authors = [\"\"]@.\
      build = \"%s\"@.@?\
    "
    name
    (if is_oracle then "oracle" else "implem")
    build_file ;
  close_out out_channel ;

  let rsc_path = Format.sprintf "%s/%s" dir rsc_dir in
  let lus_path = Format.sprintf "%s/lus" rsc_path in
  let build_file_path = Format.sprintf "%s/%s" dir build_file in

  (* Create resource / lus directory if needed. *)
  mk_dir rsc_path ;
  mk_dir lus_path ;

  (* Generate build file. *)
  let out_channel = open_out build_file_path in
  let fmt = Format.formatter_of_out_channel out_channel in
  Format.fprintf fmt "\
#![allow(non_upper_case_globals)]

use std::fs::{ OpenOptions, create_dir_all } ;
use std::path::Path ;
use std::io::{ Error, Read, Write, BufRead } ;
use std::io::Result as IoRes ;

static lus_path: & 'static str = \"rsc/lus\" ;
static tgt_path: & 'static str = \"target/doc/src/lus\" ;

fn no_access(e: Error) {
  panic!(
    \"could not access content of folder \\\"{}\\\": {}\", lus_path, e
  )
}

macro_rules! try_io {
  ($e:expr, failwith $( $fail:expr ),+) => (
    match $e {
      Ok(something) => something,
      Err(e) => panic!( $( $fail ),+ , e ),
    }
  )
}

fn copy_lus(from: & Path, to: & Path) {
  let src = try_io!(
    OpenOptions::new().read(true).open(from),
    failwith \"could not open source file {}: {}\", from.to_str().unwrap()
  ) ;
  let tgt = & mut try_io!(
    OpenOptions::new().create(true).write(true).truncate(true).open(to),
    failwith \"could not open target file {}: {}\", to.to_str().unwrap()
  ) ;

  let format = match Format::read(src) {
    Ok(format) => format,
    Err(e) => panic!(
      \"could not read source file {}: {}\", from.to_str().unwrap(), e
    ),
  } ;

  try_io!(
    write_header(tgt, from),
    failwith \"could not write target file {}: {}\", to.to_str().unwrap()
  ) ;

  try_io!(
    format.write(tgt),
    failwith \"could not write target file {}: {}\", to.to_str().unwrap()
  ) ;

  try_io!(
    write_footer(tgt),
    failwith \"could not write target file {}: {}\", to.to_str().unwrap()
  )
}

fn main() {
  let path = Path::new(lus_path) ;

  if ! path.is_dir() {
    panic!(
      \"expected to find source lustre files in \\\"{}\\\" but {} is a file\",
      lus_path, lus_path
    )
  } ;

  if ! path.exists() {
    panic!(
      \"expected to find source lustre files in \\\"{}\\\" but {} does not exist\",
      lus_path, lus_path
    )
  } ;

  // Create target dir.
  match create_dir_all(tgt_path) {
    Ok(()) => (),
    Err(e) => panic!(
      \"could not create target directory \\\"{}\\\": {}\", tgt_path, e
    ),
  } ;

  match path.read_dir() {

    Ok(entries) => for entry in entries.into_iter() {
      use std::path::PathBuf ;
      match entry {

        Ok(entry) => {
          let path_buf = entry.path() ;
          let src = path_buf.as_path() ;
          match src.file_name() {
            Some(name) => {
              let mut tgt = PathBuf::new() ;
              tgt.push(tgt_path) ;
              tgt.push(name) ;
              tgt.set_extension(\"lus.html\") ;
              copy_lus(src, & tgt)
            },
            None => (),
          }
        },

        Err(e) => no_access(e),
      }
    },

    Err(e) => no_access(e),
  }
}



struct Format {
  lines: Vec<String>,
}
impl Format {
  fn mark_lines<W: Write>(& self, w: & mut W) -> IoRes<()> {
    for n in 1..(self.lines.len() + 1) {
      try!( write!(w, \"<span id=\\\"{}\\\">{: >4}</span>\\n\", n, n) )
    } ;

    write!(w, \"</pre><pre class='rust '>\\n\")
  }
  pub fn write<W: Write>(self, w: & mut W) -> IoRes<()> {
    try!( self.mark_lines(w) ) ;
    for line in self.lines.into_iter() {
      try!( write!(w, \"{}\\n\", line) )
    } ;
    Ok(())
  }
  pub fn read<Reader: Read>(reader: Reader) -> Result<Self,String> {
    use std::io::BufReader ;
    let reader = BufReader::new(reader) ;

    let mut lines = Vec::with_capacity(1000) ;
    for line in reader.lines() {
      match line {
        Ok(line) => lines.push(line.to_string()),
        Err(e) => return Err( format!(\"could not read file: {}\", e) ),
      }
    } ;

    Ok( Format { lines: lines } )
  }
}








fn write_header<W: Write>(w: & mut W, src: & Path) -> IoRes<()> {
  let src = src.file_name().unwrap().to_str().unwrap() ;
  write!(
    w,
    \"
<!DOCTYPE html>
<html lang=\\\"en\\\">
<head>
    <meta charset=\\\"utf-8\\\">
    <meta name=\\\"viewport\\\" content=\\\"width=device-width, initial-scale=1.0\\\">
    <meta name=\\\"generator\\\" content=\\\"Kind 2\\\">
    <meta name=\\\"description\\\" content=\\\"Source to the Lustre file `{}`.\\\">
    <meta name=\\\"keywords\\\" content=\\\"rust, rustlang, rust-lang\\\">

    <title>{}.html -- source</title>

    <link rel=\\\"stylesheet\\\" type=\\\"text/css\\\" href=\\\"../../rustdoc.css\\\">
    <link rel=\\\"stylesheet\\\" type=\\\"text/css\\\" href=\\\"../../main.css\\\">



</head>
<body class=\\\"rustdoc\\\">
    <!--[if lte IE 8]>
    <div class=\\\"warning\\\">
        This old browser is unsupported and will most likely display funky
        things.
    </div>
    <![endif]-->



    <nav class=\\\"sidebar\\\">


    </nav>

    <nav class=\\\"sub\\\">
        <form class=\\\"search-form js-only\\\">
            <div class=\\\"search-container\\\">
                <input class=\\\"search-input\\\" name=\\\"search\\\"
                       autocomplete=\\\"off\\\"
                       placeholder=\\\"Click or press ‘S’ to search, ‘?’ for more options…\\\"
                       type=\\\"search\\\">
            </div>
        </form>
    </nav>

    <section id='main' class=\\\"content source\\\"><pre class=\\\"line-numbers\\\">\
    \",
    src,
    src
  )
}

fn write_footer<W: Write>(w: & mut W) -> IoRes<()> {
  write!(
    w,
    \"\
</pre>
</section>
    <section id='search' class=\\\"content hidden\\\"></section>

    <section class=\\\"footer\\\"></section>

    <aside id=\\\"help\\\" class=\\\"hidden\\\">
        <div>
            <h1 class=\\\"hidden\\\">Help</h1>

            <div class=\\\"shortcuts\\\">
                <h2>Keyboard Shortcuts</h2>

                <dl>
                    <dt>?</dt>
                    <dd>Show this help dialog</dd>
                    <dt>S</dt>
                    <dd>Focus the search field</dd>
                    <dt>&larrb;</dt>
                    <dd>Move up in search results</dd>
                    <dt>&rarrb;</dt>
                    <dd>Move down in search results</dd>
                    <dt>&#9166;</dt>
                    <dd>Go to active search result</dd>
                </dl>
            </div>

            <div class=\\\"infos\\\">
                <h2>Search Tricks</h2>

                <p>
                    Prefix searches with a type followed by a colon (e.g.
                    <code>fn:</code>) to restrict the search to a given type.
                </p>

                <p>
                    Accepted types are: <code>fn</code>, <code>mod</code>,
                    <code>struct</code>, <code>enum</code>,
                    <code>trait</code>, <code>type</code>, <code>macro</code>,
                    and <code>const</code>.
                </p>

                <p>
                    Search functions by type signature (e.g.
                    <code>vec -> usize</code> or <code>* -> vec</code>)
                </p>
            </div>
        </div>
    </aside>



    <script>
        window.rootPath = \\\"../../\\\";
        window.currentCrate = \\\"system_oracle\\\";
        window.playgroundUrl = \\\"\\\";
    </script>
    <script src=\\\"../../jquery.js\\\"></script>
    <script src=\\\"../../main.js\\\"></script>

    <script defer src=\\\"../../search-index.js\\\"></script>
</body>
</html>
    \"
  )
}
  " ;
  close_out out_channel ;
  end

(* Local Variables: *)
(* compile-command:"make -C ../../.." *)
(* End: *)
