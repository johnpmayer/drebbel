with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "rust-env";
  buildInputs = [
    rustc cargo git

    # Example Additional Dependencies
    ncurses
  ];

  # Set Environment Variables
  RUST_BACKTRACE = 1;
}
