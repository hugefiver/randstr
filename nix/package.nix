{ lib
, stdenvNoCC
, racket
, makeWrapper
, src
}:

let
  cleanedSrc = lib.cleanSourceWith {
    inherit src;
    filter = path: type:
      let
        p = toString path;
        base = baseNameOf path;
        isCompiledDir = type == "directory" && base == "compiled";
        inCompiledTree = lib.hasInfix "/compiled/" p;
        isZo = lib.hasSuffix ".zo" base;
      in
      # Keep the source tree lean when building from a VCS checkout.
      # Also drop compiled artifacts that may accidentally be committed.
      !(base == ".git" || base == ".direnv" || base == "dist" || isCompiledDir || inCompiledTree || isZo);
  };
in
stdenvNoCC.mkDerivation rec {
  pname = "randstr";
  version = "0.1.1";

  src = cleanedSrc;

  nativeBuildInputs = [ makeWrapper ];

  # No native compilation step is required; we run tests in checkPhase.
  dontBuild = true;

  doCheck = true;
  checkPhase = ''
    runHook preCheck
    ${racket}/bin/racket randstr/tests/test.rkt
    ${racket}/bin/racket randstr/tests/test-extensions.rkt
    runHook postCheck
  '';

  installPhase = ''
    runHook preInstall

    # Ship sources so the CLI can run via racket.
    mkdir -p $out/share/${pname}
    cp -R randstr $out/share/${pname}/

    mkdir -p $out/bin
    makeWrapper ${racket}/bin/racket $out/bin/randstr \
      --add-flags "$out/share/${pname}/randstr/cli/main.rkt"

    runHook postInstall
  '';

  meta = with lib; {
    description = "Generate random strings from regex-like patterns (Racket library + CLI)";
    homepage = "https://github.com/hugefiver/randstr";
    license = licenses.mit;
    platforms = platforms.unix;
    mainProgram = "randstr";
  };
}
