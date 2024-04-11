{ pkgs ? import <nixpkgs> { }, qtbase, wrapQtAppsHook }:
with pkgs;

stdenv.mkDerivation rec {
  pname = "chitubox";

  version = "1.9.5";

  src = fetchzip {
    url = "https://sac.chitubox.com/software/download.do?softwareId=17839&softwareVersionId=v${version}&fileName=CHITUBOX_V${version}.tar.gz";
    hash = "sha256-eTg6C4lnOwACbt7V5uTXpQWE1iIUyhdg4VyCJUtSn8E=";
    stripRoot=false;
  };

  #dontWrapQtApps = true;

  nativeBuildInputs = [ autoPatchelfHook wrapQtAppsHook ];

  buildInputs = [ 
    libpulseaudio
    alsa-lib
    gst_all_1.gst-plugins-base
  ];

  buildPhase = ''
    mkdir -p bin
    mv CHITUBOX bin/chitubox

    # Remove unused stuff
    rm AppRun

    # Place resources where ChiTuBox can expect to find them
    mkdir chitubox
    mv resource chitubox/

    # Configure Qt paths
    cat << EOF > bin/qt.conf
      [Paths]
      Prefix = $out
      Plugins = plugins
      Imports = qml
      Qml2Imports = qml
    EOF
  '';

  installPhase = ''
    mkdir -p $out
    mv * $out/
  '';

  meta = {
    description = "A Revolutionary Tool to Change 3D Printing Processes within One Click";
    homepage = "https://www.chitubox.com";
    license = {
      fullName = "ChiTuBox EULA";
      shortName = "ChiTuBox";
      url = "https://www.chitubox.com";
    };
  };
}
