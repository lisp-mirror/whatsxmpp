{ depot ? import <depot> {}, pkgs ? import <nixpkgs> {} }:

with depot.nix;
with depot.third_party.lisp;
let
  lispPkgs = rec {
    event-emitter = let
      src = pkgs.fetchFromGitHub {
        owner = "fukamachi";
        repo = "event-emitter";
        rev = "cb0e15f9de4c617cef3f5d5a22a41e28f9613d0b";
        sha256 = "1i18xzfr6334db9dzj0lsl7wxw1r1l0ixvn883mjbyqw0czp21h6";
      };
    in buildLisp.library {
      name = "event-emitter";
      deps = [ (buildLisp.bundled "asdf") ];
      srcs = map (f: src + ("/" + f)) [
        "src/event-emitter.lisp"
      ];
    };
    metabang-bind = let
      src = pkgs.fetchFromGitHub {
        owner = "gwkkwg";
        repo = "metabang-bind";
        rev = "9ab6e64a30261df109549d21ee7940df87db66bb";
        sha256 = "0681lp2j084w3dx02ah1vm5pk83cp5k090anwdlrjaxd1j4kbpbr";
      };
    in buildLisp.library {
      name = "metabang-bind";
      deps = [];
      srcs = map (f: src + ("/dev/" + f)) [
        "packages.lisp"
        "macros.lisp"
        "bind.lisp"
        "binding-forms.lisp"
      ];
    };
    qbase64 = let
      src = pkgs.fetchFromGitHub {
        owner = "chaitanyagupta";
        repo = "qbase64";
        rev = "6374899aec189600e6b7b77c89009d0835154b93";
        sha256 = "1dir0s70ca3hagxv9x15zq4p4ajgl7jrcgqsza2n2y7iqbxh0dwi";
      };
    in buildLisp.library {
      name = "qbase64";
      deps = [ trivial-gray-streams metabang-bind ];
      srcs = map (f: src + ("/" + f)) [
        "package.lisp"
        "utils.lisp"
        "stream-utils.lisp"
        "qbase64.lisp"
      ];
    };
    com-google-base = let
      src = pkgs.fetchFromGitHub {
        owner = "brown";
        repo = "base";
        rev = "498fd7224748a1cceaa6127edcedab6e3563aa84";
        sha256 = "0af9gz2rnw4wbdgx3v6bpyfjn04700irdvcp0s41jdp8djpip8f7";
      };
    in buildLisp.library {
      name = "com.google.base";
      deps = [];
      srcs = map (f: src + ("/" + f)) [
        "package.lisp"
        "optimize.lisp"
        "syntax.lisp"
        "error.lisp"
        "type.lisp"
        "octet.lisp"
        "sequence.lisp"
      ];
    };
    protobuf-src = pkgs.fetchFromGitHub {
      owner = "brown";
      repo = "protobuf";
      rev = "f55c6f51848ff1f9d523fb293fbe99c076a34a95";
      sha256 = "1frm5s7wsi4nh1adc7qkph5r1cf24fgvbg0xvsyv7cmgg5r3bibi";
    };
    varint = buildLisp.library {
      name = "varint";
      deps = [ com-google-base nibbles ];
      srcs = map (f: protobuf-src + ("/varint/" + f)) [
        "package.lisp"
        "varint.lisp"
      ];
    };
    protobuf = buildLisp.library {
      name = "protobuf";
      deps = [ com-google-base varint ];
      srcs = map (f: protobuf-src + ("/" + f)) [
        "package.lisp"
        "protocol-buffer.lisp"
        "wire-format.lisp"
      ];
    };
    trivial-utf-8 = let
      src = builtins.fetchTarball {
        url = "https://gitlab.common-lisp.net/trivial-utf-8/trivial-utf-8/-/archive/4d427cfbb1c452436a0efb71c3205c9da67f718f/trivial-utf-8-4d427cfbb1c452436a0efb71c3205c9da67f718f.tar.gz";
        sha256 = "1jz27gz8gvqdmvp3k9bxschs6d5b3qgk94qp2bj6nv1d0jc3m1l1";
      };
    in buildLisp.library {
      name = "trivial-utf-8";
      deps = [ (buildLisp.bundled "asdf") ];
      srcs = map (f: src + ("/" + f)) [
        "trivial-utf-8.lisp"
      ];
    };
    salza2 = let
      src = builtins.fetchTarball {
        url = "https://www.xach.com/lisp/salza2.tgz";
        sha256 = "0p38rj4gq7j5k807php7hrz7l2zyyfshv8i9yms7i8lkgg3433ki";
      };
    in buildLisp.library {
      name = "salza2";
      deps = [];
      srcs = map (f: src + ("/" + f)) [
        "package.lisp"
        "reset.lisp"
        "specials.lisp"
        "types.lisp"
        "checksum.lisp"
        "adler32.lisp"
        "crc32.lisp"
        "chains.lisp"
        "bitstream.lisp"
        "matches.lisp"
        "compress.lisp"
        "huffman.lisp"
        "closures.lisp"
        "compressor.lisp"
        "utilities.lisp"
        "zlib.lisp"
        "gzip.lisp"
        "user.lisp"
      ];
    };
    zpng = let
      src = builtins.fetchTarball {
        url = "https://www.xach.com/lisp/zpng.tgz";
        sha256 = "0b3ag3jhl3z7kdls3ahdsdxsfhhw5qrizk769984f4wkxhb69rcm";
      };
    in buildLisp.library {
      name = "zpng";
      deps = [ salza2 ];
      srcs = map (f: src + ("/" + f)) [
        "package.lisp"
        "specials.lisp"
        "utils.lisp"
        "chunk.lisp"
        "conditions.lisp"
        "png.lisp"
      ];
    };
    zpb-exif = let
      src = builtins.fetchTarball {
        url = "https://www.xach.com/lisp/zpb-exif.tgz";
        sha256 = "15s227jhby55cisz14xafb0p1ws2jmrg2rrbbd00lrb97im84hy6";
      };
    in buildLisp.library {
      name = "zpb-exif";
      deps = [ salza2 ];
      srcs = map (f: src + ("/" + f)) [
        "exif.lisp"
      ];
    };
    skippy = let
      src = builtins.fetchTarball {
        url = "https://www.xach.com/lisp/skippy.tgz";
        sha256 = "1n8925qz19w00qc67z3hc97fpmfhi0r54dd50fzqm24vhyb7qwc2";
      };
    in buildLisp.library {
      name = "skippy";
      deps = [ salza2 ];
      srcs = map (f: src + ("/" + f)) [
        "package.lisp"
        "conditions.lisp"
        "types.lisp"
        "bitstream.lisp"
        "lzw.lisp"
        "color-table.lisp"
        "canvas.lisp"
        "data-stream.lisp"
        "image.lisp"
        "gif89a.lisp"
        "load-gif.lisp"
      ];
    };
    puri = let
      src = builtins.fetchTarball {
        url = "http://files.kpe.io/puri/puri-1.5.7.tar.gz";
        sha256 = "0bdjcjr2wmwxhv4sr20hh6dhakjdal5hiwzpmvbwzgl394byk100";
      };
    in buildLisp.library {
      name = "puri";
      deps = [];
      srcs = map (f: src + ("/" + f)) [
        "src.lisp"
      ];
    };
    cl-utilities = let
      src = builtins.fetchTarball {
        url = "https://common-lisp.net/project/cl-utilities/cl-utilities-latest.tar.gz";
        sha256 = "1dmbkdr8xm2jw5yx1makqbf1ypqbm0hpkd7zyknxv3cblvz0a87w";
      };
    in buildLisp.library {
      name = "cl-utilities";
      deps = [];
      srcs = map (f: src + ("/" + f)) [
        "package.lisp"
        "with-unique-names.lisp"
        "once-only.lisp"
        "compose.lisp"
        "split-sequence.lisp"
        "extremum.lisp"
        "read-delimited.lisp"
        "expt-mod.lisp"
        "collecting.lisp"
        "rotate-byte.lisp"
        "copy-array.lisp"
      ];
    };
    trivial-timers = let
      src = builtins.fetchTarball {
        url = "http://releases.unknownlamer.org/trivial-timers/trivial-timers_latest.tar.gz";
        sha256 = "0nxr0wb9n2jh6k3cj9ddx2hvgvr4bqb8k587cmj6pjsaa62344mb";
      };
    in buildLisp.library {
      name = "trivial-timers";
      deps = [];
      srcs = map (f: src + ("/" + f)) [
        "packages.lisp"
        "timers-sbcl.lisp"
      ];
    };
    quri = let
      src = pkgs.fetchFromGitHub {
        owner = "fukamachi";
        repo = "quri";
        rev = "b39ec54a07062334cfa37b31d14e39115921ffe0";
        sha256 = "1nw0m5fm84x9x8mx5pp6l2bhmpzmmgy4rwj3bkkg4qsp7h6a1ak7";
      };
      etld-nix = import ./nix/etld.nix { tld-names = src + "/data/effective_tld_names.dat"; };
    in buildLisp.library {
      name = "quri";
      deps = [ (buildLisp.bundled "sb-cltl2") (buildLisp.bundled "asdf") babel alexandria split-sequence cl-utilities ];
      srcs = map (f: src + ("/src/" + f)) [
        "../quri.asd"
        "port.lisp"
        "util.lisp"
      ] ++ [
        etld-nix
      ] ++ map (f: src + ("/src/" + f)) [
        "error.lisp"
        "encode.lisp"
        "decode.lisp"
        "parser.lisp"
        "uri.lisp"
        "uri/ftp.lisp"
        "uri/http.lisp"
        "uri/ldap.lisp"
        "uri/file.lisp"
        "domain.lisp"
        "quri.lisp"
      ];
    };
    trivial-mimes = let
      mime-types = ./nix/mime.types;
      mime-types-lisp = import ./nix/trivial-mimes.nix { mime-types = mime-types; };
    in buildLisp.library {
      name = "trivial-mimes";
      deps = [ (buildLisp.bundled "uiop") ];
      native = [];
      srcs = [ mime-types-lisp ];
    };
    cl-sqlite = let
      src = pkgs.fetchFromGitHub {
        owner = "TeMPOraL";
        repo = "cl-sqlite";
        rev = "be2fcc193f98e3d5bdc85958a806d612cc48740c";
        sha256 = "08iv7b4m0hh7qx2cvq4f510nrgdld0vicnvmqsh9w0fgrcgmyg4k";
      };
    in buildLisp.library {
      name = "cl-sqlite";
      deps = [ iterate cffi ];
      native = [ pkgs.sqlite ];
      srcs = map (f: src + ("/" + f)) [
        "sqlite-ffi.lisp"
        "cache.lisp"
        "sqlite.lisp"
      ];
    };
    fast-websocket = let
      src = pkgs.fetchFromGitHub {
        owner = "fukamachi";
        repo = "fast-websocket";
        rev = "7087d9cf4c3f7da4c68a275ad79eda853ac6f1b9";
        sha256 = "0hmp9mzz7svl0csh9n2954ddpcbngl87jfv32il2q8kkawiycij7";
      };
    in buildLisp.library {
      name = "fast-websocket";
      deps = [ fast-io trivial-utf-8 alexandria ];
      srcs = map (f: src + ("/src/" + f)) [
        "ws.lisp"
        "payload.lisp"
        "constants.lisp"
        "error.lisp"
        "compose.lisp"
        "parser.lisp"
        "fast-websocket.lisp"
      ];
    };
    fast-io = let
      src = pkgs.fetchFromGitHub {
        owner = "rpav";
        repo = "fast-io";
        rev = "603f4903dd74fb221859da7058ae6ca3853fe64b";
        sha256 = "00agvc0xx4w715i6ach05p995zpcpghn04xc06zyci06q677vw3n";
      };
    in buildLisp.library {
      name = "fast-io";
      deps = [ alexandria trivial-gray-streams ];
      srcs = map (f: src + ("/src/" + f)) [
        "package.lisp"
        "types.lisp"
        "io.lisp"
        "gray.lisp"
      ];
    };
    proc-parse = let
      src = pkgs.fetchFromGitHub {
        owner = "fukamachi";
        repo = "proc-parse";
        rev = "3afe2b76f42f481f44a0a495256f7abeb69cef27";
        sha256 = "07vbj26bfq4ywlcmamsqyac29rsdsa8lamjqx1ycla1bcvgmi4w2";
      };
    in buildLisp.library {
      name = "proc-parse";
      deps = [ alexandria babel (buildLisp.bundled "sb-cltl2") ];
      srcs = map (f: src + ("/src/" + f)) [
        "proc-parse.lisp"
      ];
    };
    xsubseq = let
      src = pkgs.fetchFromGitHub {
        owner = "fukamachi";
        repo = "xsubseq";
        rev = "5ce430b3da5cda3a73b9cf5cee4df2843034422b";
        sha256 = "1xz79q0p2mclf3sqjiwf6izdpb6xrsr350bv4mlmdlm6rg5r99px";
      };
    in buildLisp.library {
      name = "xsubseq";
      deps = [ (buildLisp.bundled "sb-cltl2") ];
      srcs = map (f: src + ("/src/" + f)) [
        "xsubseq.lisp"
      ];
    };
    uuid = let
      src = pkgs.fetchFromGitHub {
        owner = "dardoria";
        repo = "uuid";
        rev = "f0052f34a006ec995086aa3b2e42182a178fe228";
        sha256 = "1ncwhyw0zggwpkzjsw7d4pkrlldi34xvb69c0bzxmyz2krg8rpx0";
      };
    in buildLisp.library {
      name = "uuid";
      deps = [ ironclad trivial-utf-8 ];
      srcs = map (f: src + ("/" + f)) [
        "uuid.lisp"
      ];
    };
    smart-buffer = let
      src = pkgs.fetchFromGitHub {
        owner = "fukamachi";
        repo = "smart-buffer";
        rev = "09b9a9a0b3abaa37abe9a730f5aac2643dca4e62";
        sha256 = "0qz1zzxx0wm5ff7gpgsq550a59p0qj594zfmm2rglj97dahj54l7";
      };
    in buildLisp.library {
      name = "smart-buffer";
      deps = [ xsubseq flexi-streams (buildLisp.bundled "uiop") ];
      srcs = map (f: src + ("/src/" + f)) [
        "smart-buffer.lisp"
      ];
    };
    fast-http = let
      src = pkgs.fetchFromGitHub {
        owner = "fukamachi";
        repo = "fast-http";
        rev = "502a37715dcb8544cc8528b78143a942de662c5a";
        sha256 = "0al2g7g219jjljsf7b23pbilpgacxy5as5gs2nqf76b5qni396mi";
      };
    in buildLisp.library {
      name = "fast-http";
      deps = [ alexandria cl-utilities proc-parse babel xsubseq smart-buffer ];
      srcs = map (f: src + ("/src/" + f)) [
        "http.lisp"
        "byte-vector.lisp"
        "error.lisp"
        "util.lisp"
        "parser.lisp"
        "multipart-parser.lisp"
        "fast-http.lisp"
      ];
    };
    wsd-src = pkgs.fetchFromGitHub {
      owner = "eeeeeta";
      repo = "websocket-driver";
      rev = "03fdb0684c50fe77cf186d208c671bd9a1575985";
      sha256 = "06plrbk7sbz456hkxsd97ib7xvjp484d18l0r11pqjd1659smjb3";
    };
    wsd-base = buildLisp.library {
      name = "websocket-driver-base";
      deps = [ fast-websocket fast-io event-emitter ironclad cl-base64 split-sequence bordeaux-threads ];
      srcs = map (f: wsd-src + ("/src/" + f)) [
        "util.lisp"
        "ws/base.lisp"
        "driver.lisp"
      ];
    };
    wsd-client = buildLisp.library {
      name = "websocket-driver-client";
      deps = [ wsd-base usocket cl-plus-ssl fast-io fast-websocket cl-base64 trivial-utf-8 ironclad quri fast-http ];
      srcs = map (f: wsd-src + ("/src/" + f)) [
        "ws/client.lisp"
        "client.lisp"
      ];
    };
    closure-common = let
      src = pkgs.fetchFromGitHub {
        owner = "sharplispers";
        repo = "closure-common";
        rev = "e3c5f5f454b72b01b89115e581c3c52a7e201e5c";
        sha256 = "0k5r2qxn122pxi301ijir3nayi9sg4d7yiy276l36qmzwhp4mg5n";
      };
      enable-character-runes = pkgs.writeText "character-runes.lisp" "(push :rune-is-character *features*)";
    in buildLisp.library {
      name = "closure-common";
      deps = [ trivial-gray-streams babel ];
      srcs = [
        enable-character-runes
      ] ++ map (f: src + ("/" + f)) [
        "package.lisp"
        "definline.lisp"
        "characters.lisp"
        "syntax.lisp"
        "encodings.lisp"
        "encodings-data.lisp"
        "xstream.lisp"
        "ystream.lisp"
        "hax.lisp"
      ];
    };
    cxml = let
      src = pkgs.fetchFromGitHub {
        owner = "sharplispers";
        repo = "cxml";
        rev = "8701da08ba4aac30891b8d2005edb018c1d3d796";
        sha256 = "18fls3bx7vmnxfa6qara8fxp316d8kb3izar0kysvqg6l0a45a51";
      };
    in buildLisp.library {
      name = "cxml";
      deps = [ (buildLisp.bundled "asdf") closure-common puri trivial-gray-streams ];
      srcs = [
        (src + "/cxml.asd")
      ] ++ map (f: src + ("/xml/" + f)) [
        "package.lisp"
        "util.lisp"
        "sax-handler.lisp"
        "xml-name-rune-p.lisp"
        "split-sequence.lisp"
        "xml-parse.lisp"
        "unparse.lisp"
        "xmls-compat.lisp"
        "recoder.lisp"
        "xmlns-normalizer.lisp"
        "space-normalizer.lisp"
        # "catalog.lisp"
        "sax-proxy.lisp"
        "atdoc-configuration.lisp"
      ] ++ map (f: src + ("/dom/" + f)) [
        "package.lisp"
        "dom-impl.lisp"
        "dom-builder.lisp"
        "dom-sax.lisp"
      ] ++ map (f: src + ("/klacks/" + f)) [
        "package.lisp"
        "klacks.lisp"
        "klacks-impl.lisp"
        "tap-source.lisp"
      ];
    };
    cl-qrencode = let
      src = pkgs.fetchFromGitHub {
        owner = "jnjcc";
        repo = "cl-qrencode";
        rev = "0de2d8a3877b499a9a0bbb0a9e1247056ae4311e";
        sha256 = "1l5k131dchbf6cj8a8xqa731790p01p3qa1kdy2wa9dawy3ymkxr";
      };
    in buildLisp.library {
      name = "cl-qrencode";
      deps = [ zpng ];
      srcs = map (f: src + ("/" + f)) [
        "packages.lisp"
        "utils/util.lisp"
        "rs-ecc/galois.lisp"
        "rs-ecc/bch-ecc.lisp"
        "rs-ecc/rs-ecc.lisp"
        "qrencode/modes.lisp"
        "qrencode/qrspec.lisp"
        "qrencode/input.lisp"
        "qrencode/bstream.lisp"
        "qrencode/codeword.lisp"
        "qrencode/matrix.lisp"
        "qrencode/mask.lisp"
        "qrencode/encode.lisp"
        "image/png.lisp"
      ];
    };
    vom = let
      src = pkgs.fetchFromGitHub {
        owner = "orthecreedence";
        repo = "vom";
        rev = "1aeafeb5b74c53741b79497e0ef4acf85c92ff24";
        sha256 = "0536kppj76ax4lrxhv42npkfjsmx45km2g439vf9jmw3apinz9cy";
      };
    in buildLisp.library {
      name = "vom";
      deps = [];
      srcs = map (f: src + ("/" + f)) [
        "vom.lisp"
      ];
    };
    blackbird = let
      src = pkgs.fetchFromGitHub {
        owner = "orthecreedence";
        repo = "blackbird";
        rev = "d361f81c1411dec07f6c2dcb11c78f7aea9aaca8";
        sha256 = "0xfds5yaya64arzr7w1x38karyz11swzbhxx1afldpradj9dh19c";
      };
    in buildLisp.library {
      name = "blackbird";
      deps = [ vom ];
      srcs = map (f: src + ("/" + f)) [
        "package.lisp"
        "syntax.lisp"
        "promise.lisp"
        "util.lisp"
      ];
    };
    com-gigamonkeys-binary-data = let
      src = pkgs.fetchFromGitHub {
        owner = "gigamonkey";
        repo = "monkeylib-binary-data";
        rev = "22e908976d7f3e2318b7168909f911b4a00963ee";
        sha256 = "072v417vmcnvmyh8ddq9vmwwrizm7zwz9dpzi14qy9nsw8q649zw";
      };
    in buildLisp.library {
      name = "com.gigamonkeys.binary-data";
      deps = [ alexandria ];
      srcs = map (f: src + ("/" + f)) [
        "packages.lisp"
        "binary-data.lisp"
        "common-datatypes.lisp"
      ];
    };
    ieee-floats = let
      src = pkgs.fetchFromGitHub {
        owner = "marijnh";
        repo = "ieee-floats";
        rev = "566b51a005e81ff618554b9b2f0b795d3b29398d";
        sha256 = "1xyj49j9x3lc84cv3dhbf9ja34ywjk1c46dklx425fxw9mkwm83m";
      };
    in buildLisp.library {
      name = "ieee-floats";
      deps = [];
      srcs = map (f: src + ("/" + f)) [
        "ieee-floats.lisp"
      ];
    };
    cl-jpeg = let
      src = pkgs.fetchFromGitHub {
        owner = "sharplispers";
        repo = "cl-jpeg";
        rev = "ec557038128df6895fbfb743bfe8faf8ec2534af";
        sha256 = "1bkkiqz8fqldlj1wbmrccjsvxcwj98h6s4b6gslr3cg2wmdv5xmy";
      };
    in buildLisp.library {
      name = "cl-jpeg";
      deps = [];
      srcs = map (f: src + ("/" + f)) [
        "package.lisp"
        "jpeg.lisp"
        "io.lisp"
      ];
    };
    deflate = let
      src = pkgs.fetchFromGitHub {
        owner = "pmai";
        repo = "deflate";
        rev = "fb940e63b89a6c4d168153dbf046552e106eb8a5";
        sha256 = "1jpdjnxh6cw2d8hk70r2sxn92is52s9b855irvwkdd777fdciids";
      };
    in buildLisp.library {
      name = "deflate";
      deps = [];
      srcs = map (f: src + ("/" + f)) [
        "deflate.lisp"
      ];
    };
    trivial-features = let
      src = pkgs.fetchFromGitHub {
        owner = "trivial-features";
        repo = "trivial-features";
        rev = "e7bb968d1e0b00aaf06e0671a866a81dbfe99bee";
        sha256 = "1iczrsl561fz9f71dzals16749fccznm4jn8nmxnqas1qk7b331k";
      };
    in buildLisp.library {
      name = "trivial-features";
      deps = [];
      srcs = map (f: src + ("/src/" + f)) [
        "tf-sbcl.lisp"
      ];
    };
    opticl-core = let
      src = pkgs.fetchFromGitHub {
        owner = "slyrus";
        repo = "opticl-core";
        rev = "b7cd13d26df6b824b216fbc360dc27bfadf04999";
        sha256 = "0458bllabcdjghfrqx6aki49c9qmvfmkk8jl75cfpi7q0i12kh95";
      };
    in buildLisp.library {
      name = "opticl-core";
      deps = [ alexandria ];
      srcs = map (f: src + ("/" + f)) [
        "package.lisp"
        "opticl-core.lisp"
      ];
    };
    retrospectiff = let
      src = pkgs.fetchFromGitHub {
        owner = "slyrus";
        repo = "retrospectiff";
        rev = "c2a69d77d5010f8cdd9045b3e36a08a73da5d321";
        sha256 = "0qsn9hpd8j2kp43dk05j8dczz9zppdff5rrclbp45n3ksk9inw8i";
      };
    in buildLisp.library {
      name = "retrospectiff";
      deps = [ com-gigamonkeys-binary-data flexi-streams ieee-floats cl-jpeg deflate opticl-core ];
      srcs = map (f: src + ("/" + f)) [
        "package.lisp"
        "constants.lisp"
        "globals.lisp"
        "util.lisp"
        "bit-array.lisp"
        "lzw.lisp"
        "jpeg.lisp"
        "deflate.lisp"
        "packbits.lisp"
        "compression.lisp"
        "binary-types.lisp"
        "ifd.lisp"
        "tiff-image.lisp"
        "retrospectiff.lisp"
        "retrospectiff2.lisp"
      ];
    };
    cl-tga = let
      src = pkgs.fetchFromGitHub {
        owner = "fisxoj";
        repo = "cl-tga";
        rev = "4dc2f7b8a259b9360862306640a07a23d4afaacc";
        sha256 = "03k3npmn0xd3fd2m7vwxph82av2xrfb150imqrinlzqmzvz1v1br";
      };
    in buildLisp.library {
      name = "cl-tga";
      deps = [];
      srcs = map (f: src + ("/" + f)) [
        "package.lisp"
        "cl-tga.lisp"
      ];
    };
    mmap = let
      src = pkgs.fetchFromGitHub {
        owner = "Shinmera";
        repo = "mmap";
        rev = "ba2e98c67e25f0fb8ff838238561120a23903ce7";
        sha256 = "0qd0xp20i1pcfn12kkapv9pirb6hd4ns7kz4zf1mmjwykpsln96q";
      };
    in buildLisp.library {
      name = "mmap";
      deps = [ cffi ];
      srcs = map (f: src + ("/" + f)) [
        "package.lisp"
        "generic.lisp"
        "posix.lisp"
      ];
    };
    static-vectors = let
      src = pkgs.fetchFromGitHub {
        owner = "sionescu";
        repo = "static-vectors";
        rev = "67f2ed0da2244f3c2a69d3440eddcc14a3ad33f0";
        sha256 = "0prdwkyggr9wqwr7blhrb3hprsvbcgwn7144f7v4iy7i8621d8pq";
      };
    in buildLisp.library {
      name = "static-vectors";
      deps = [ alexandria cffi ];
      srcs = map (f: src + ("/src/" + f)) [
        "pkgdcl.lisp"
        "constantp.lisp"
        "impl-sbcl.lisp"
        "constructor.lisp"
        "cffi-type-translator.lisp"
      ];
    };
    swap-bytes = let
      src = pkgs.fetchFromGitHub {
        owner = "sionescu";
        repo = "swap-bytes";
        rev = "253ab928b91b8a1c3cea0434e87b8da5ce3c6014";
        sha256 = "1rs1166rabdlws4pyvsrwl32x476dh2yw15p56097mp8ixmcb0ap";
      };
    in buildLisp.library {
      name = "swap-bytes";
      deps = [ trivial-features ];
      srcs = map (f: src + ("/" + f)) [
        "package.lisp"
        "sbcl-defknowns.lisp"
        "sbcl-vops.lisp"
        "sbcl.lisp"
        "network.lisp"
        "endianness.lisp"
      ];
    };
    threebz = let
      src = pkgs.fetchFromGitHub {
        owner = "3b";
        repo = "3bz";
        rev = "d6119083b5e0b0a6dd3abc2877936c51f3f3deed";
        sha256 = "0fyxzyf2b6sc0w8d9g4nlva861565z6f3xszj0lw29x526dd9rhj";
      };
    in buildLisp.library {
      name = "3bz";
      deps = [ alexandria cffi mmap trivial-features nibbles babel ];
      srcs = map (f: src + ("/" + f)) [
        "package.lisp"
        "tuning.lisp"
        "util.lisp"
        "constants.lisp"
        "types.lisp"
        "huffman-tree.lisp"
        "ht-constants.lisp"
        "io-common.lisp"
        "io-mmap.lisp"
        "io.lisp"
        "deflate.lisp"
        "checksums.lisp"
        "zlib.lisp"
        "gzip.lisp"
        "api.lisp"
      ];
    };
    pngload = let
      src = pkgs.fetchFromGitHub {
        owner = "bufferswap";
        repo = "pngload";
        rev = "b2e56733dd5d86a56b20c665676b86e566b4e223";
        sha256 = "15dkm3ba7byxk8qs6d3xnd58ybvjl6cjz75392z5fq5cqygbgfq5";
      };
    in buildLisp.library {
      name = "pngload";
      deps = [ threebz alexandria cffi mmap parse-float static-vectors swap-bytes (buildLisp.bundled "uiop") zpb-exif ];
      srcs = map (f: src + ("/src/" + f)) [
        "package.lisp"
        "common.lisp" # aha!
        "source.lisp"
        "source-ffi.lisp"
        "properties.lisp"
        "chunk.lisp"
        "chunk-types.lisp"
        "conditions.lisp"
        "datastream.lisp"
        "deinterlace.lisp"
        "decode.lisp"
        "metadata.lisp"
        "png.lisp"
        "png-mmap.lisp"
      ];
    };
    opticl = let
      src = pkgs.fetchFromGitHub {
        owner = "slyrus";
        repo = "opticl";
        rev = "438881ae779fa4b113308a3c5c96783fd9618e02";
        sha256 = "13sv7n1ry8yp3fawvpf3y3kf7abbqxqmk8zpx349k3wh063i7l1l";
      };
    in buildLisp.library {
      name = "opticl";
      deps = [ alexandria retrospectiff zpng pngload cl-jpeg skippy opticl-core cl-tga ];
      srcs = map (f: src + ("/" + f)) [
        "package.lisp"
        "coerce.lisp"
        # "colors.lisp"
        "imageops.lisp"
        "invert.lisp"
        "transform.lisp"
        "convolve.lisp"
        "morphology.lisp"
        "gamma.lisp"
        "shapes.lisp"
        "tiff.lisp"
        "jpeg.lisp"
        "png.lisp"
        "pngload.lisp"
        "pnm.lisp"
        "gif.lisp"
        "tga.lisp"
        "io.lisp"
        "cluster.lisp"
        "thresholding.lisp"
      ];
    };
    nibbles = let
      src = pkgs.fetchFromGitHub {
        owner = "sharplispers";
        repo = "nibbles";
        rev = "2b99f103f84f0a6dcbc8212f7763d6b08b13e3fa";
        sha256 = "06ckb4hjl4kca9dwldqrz6gxmfic47makahq3p3ysv71zncr9zgm";
      };
    in buildLisp.library {
      name = "nibbles";
      deps = [ trivial-gray-streams metabang-bind ];
      srcs = map (f: src + ("/" + f)) [
        "package.lisp"
        "types.lisp"
        "macro-utils.lisp"
        "vectors.lisp"
        "streams.lisp"
        "sbcl-opt/fndb.lisp"
        "sbcl-opt/nib-tran.lisp"
        "sbcl-opt/x86-vm.lisp"
        "sbcl-opt/x86-64-vm.lisp"
      ];
    };
    ironclad = let
      src = pkgs.fetchFromGitHub {
        owner = "sharplispers";
        repo = "ironclad";
        rev = "c3aa33080621abc10fdb0f34acc4655cc4e982a6";
        sha256 = "0k4bib9mbrzalbl9ivkw4a7g4c7bbad1l5jw4pzkifqszy2swkr5";
      };
    in buildLisp.library {
      name = "ironclad";
      deps = [ (buildLisp.bundled "sb-posix") (buildLisp.bundled "sb-rotate-byte") bordeaux-threads ];
      srcs = map (f: src + ("/src/" + f)) [
        "package.lisp"
        "macro-utils.lisp"
        "generic.lisp"
        "conditions.lisp"
        "util.lisp"
      ] ++ map (f: src + ("/src/prng/" + f)) [
        "prng.lisp"
        "generator.lisp"
        "os-prng.lisp"
        "fortuna.lisp"
      ] ++ map (f: src + ("/src/opt/" + f)) [
        "sbcl/fndb.lisp"
        "sbcl/x86oid-vm.lisp"
        "sbcl/cpu-features.lisp"
      ] ++ map (f: src + ("/src/" + f)) [
        "common.lisp" # aha!
      ] ++ map (f: src + ("/src/ciphers/" + f)) [
        "cipher.lisp"
        "padding.lisp"
        "modes.lisp"
        "salsa20.lisp"
        "aes.lisp"
        "arcfour.lisp"
        "aria.lisp"
        "blowfish.lisp"
        "camellia.lisp"
        "cast5.lisp"
        "chacha.lisp"
        "des.lisp"
        "idea.lisp"
        "keystream.lisp"
        "kalyna.lisp"
        "kuznyechik.lisp"
        "make-cipher.lisp"
        "misty1.lisp"
        "rc2.lisp"
        "rc5.lisp"
        "rc6.lisp"
        "seed.lisp"
        "serpent.lisp"
        "sm4.lisp"
        "sosemanuk.lisp"
        "square.lisp"
        "tea.lisp"
        "threefish.lisp"
        "twofish.lisp"
        "xchacha.lisp"
        "xor.lisp"
        "xsalsa20.lisp"
        "xtea.lisp"
      ] ++ map (f: src + ("/src/digests/" + f)) [
        "digest.lisp"
        "adler32.lisp"
        "blake2.lisp"
        "blake2s.lisp"
        "crc24.lisp"
        "crc32.lisp"
        "groestl.lisp"
        "jh.lisp"
        "kupyna.lisp"
        "md2.lisp"
        "md4.lisp"
        "md5.lisp"
        "md5-lispworks-int32.lisp"
        "ripemd-128.lisp"
        "ripemd-160.lisp"
        "sha1.lisp"
        "sha256.lisp"
        "sha3.lisp"
        "sha512.lisp"
        "skein.lisp"
        "sm3.lisp"
        "streebog.lisp"
        "tiger.lisp"
        "tree-hash.lisp"
        "whirlpool.lisp"
      ] ++ map (f: src + ("/src/public-key/" + f)) [
        "public-key.lisp"
        "curve25519.lisp"
        "curve448.lisp"
        "dsa.lisp"
        "ed25519.lisp"
        "ed448.lisp"
        "elgamal.lisp"
        "pkcs1.lisp"
        "rsa.lisp"
      ] ++ map (f: src + ("/src/macs/" + f)) [
        "mac.lisp"
        "blake2-mac.lisp"
        "blake2s-mac.lisp"
        "cmac.lisp"
        "hmac.lisp"
        "gmac.lisp"
        "poly1305.lisp"
        "siphash.lisp"
        "skein-mac.lisp"
      ] ++ map (f: src + ("/src/kdf/" + f)) [
        "kdf-common.lisp"
        "hmac.lisp"
        "pkcs5.lisp"
        "argon2.lisp"
        "password-hash.lisp"
        "scrypt.lisp"
      ] ++ map (f: src + ("/src/aead/" + f)) [
        "aead.lisp"
        "eax.lisp"
        "etm.lisp"
        "gcm.lisp"
      ] ++ map (f: src + ("/src/" + f)) [
        "math.lisp"
        "octet-stream.lisp"
      ];
    };
  };
  whatscl = import ../whatscl { lispPkgs = lispPkgs; };
in
with lispPkgs;
buildLisp.program {
  name = "whatsxmpp";
  deps = [ whatscl blackbird cxml uuid cl-sqlite trivial-mimes drakma cl-qrencode trivial-backtrace opticl ];
  srcs = map (f: ./. + ("/" + f)) [
    "packages.lisp"
    "utils.lisp"
    "namespaces.lisp"
    "component.lisp"
    "xmpp.lisp"
    "xep-0030.lisp"
    "xep-0363.lisp"
    "xep-0115.lisp"
    "xep-0313.lisp"
    "sqlite.lisp"
    "db.lisp"
    "media.lisp"
    "message.lisp"
    "stuff.lisp"
  ];
  extraInit = "(sb-ext:restrict-compiler-policy 'safety 3 3)";
  main = "whatsxmpp::main";
}
