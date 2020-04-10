## basic OCaml and opam installation

full_apt_version () {
  package=$1
  version=$2
  case "${version}" in
      latest) echo -n "${package}" ;;
      *) echo -n "${package}="
         apt-cache show "$package" \
             | sed -n "s/^Version: \(${version}\)/\1/p" \
             | head -1
  esac
}

set -uex


SYS_OCAML_VERSION=4.05
# Default opam is the latest release of opam 2
OPAM_VERSION=${OPAM_VERSION:-2}
OPAM_INIT=${OPAM_INIT:-true}
OCAML_BETA=${OCAML_BETA:-disable}

OPAM_LATEST_RELEASE=2.0.6

case $OPAM_VERSION in
    2|2.0) OPAM_VERSION=$OPAM_LATEST_RELEASE;;
    1.*) echo "Opam version '$OPAM_VERSION' is not supported"; exit 1;;
esac

if [ "$TRAVIS_OS_NAME" = "osx" ] ; then
    brew update &> /dev/null
    BREW_OPAM_VERSION=$(brew info opam --json=v1 | sed -e 's/.*"versions":{[^}]*"stable":"//' -e 's/".*//')
    if [ "$OPAM_VERSION" != "$BREW_OPAM_VERSION" ] ; then
        set +x
        echo -e "[\e[0;31mWARNING\e[0m] Ignored OPAM_VERSION=$OPAM_VERSION; interpreted as \"$BREW_OPAM_VERSION\"" >&2
        echo -e "[\e[0;31mWARNING\e[0m] opam 2 is installed via Homebrew" >&2
        set -x
    fi
    OPAM_VERSION="$BREW_OPAM_VERSION"
fi

if [ "$OPAM_VERSION" != "$OPAM_LATEST_RELEASE" ] ; then
    set +x
    echo -e "[\e[0;31mWARNING\e[0m] Out-of-date opam $OPAM_VERSION requested" >&2
    echo -e "[\e[0;31mWARNING\e[0m] Latest release is $OPAM_LATEST_RELEASE" >&2
    set -x
fi

if [ "${INSTALL_LOCAL+x}" = x ] ; then
  if [ "$TRAVIS_OS_NAME" = osx ] ; then
    echo INSTALL_LOCAL not permitted for macOS targets
    exit 1
  fi

  if [ "${OPAM_SWITCH:=ocaml-system}" != ocaml-system ] ; then
    echo "INSTALL_LOCAL requires OPAM_SWITCH=ocaml-system (or unset/null)"
    exit 1
  fi
fi

# the base opam repository to use for bootstrapping and catch-all namespace
BASE_REMOTE=${BASE_REMOTE:-git://github.com/ocaml/opam-repository}

# whether we need a new gcc and binutils
UPDATE_GCC_BINUTILS=${UPDATE_GCC_BINUTILS:-"0"}

# Install Xenial remotes
UBUNTU_XENIAL=${UBUNTU_XENIAL:-"0"}

# Install XQuartz on OSX
INSTALL_XQUARTZ=${INSTALL_XQUARTZ:-"false"}

APT_UPDATED=0

add_ppa () {
    if [ "$TRAVIS_OS_NAME" = "linux" ] ; then
        APT_UPDATED=0
        sudo add-apt-repository --yes ppa:$1
    fi
}

apt_install () {
    if [ "$TRAVIS_OS_NAME" = "linux" ] ; then
        if [ "$APT_UPDATED" -eq 0 ] ; then
            APT_UPDATED=1
            sudo apt-get update -qq
        fi
        sudo apt-get install --no-install-recommends -y "$@"
    fi
}

install_ocaml () {
    apt_install \
         ocaml ocaml-base ocaml-native-compilers ocaml-compiler-libs \
         ocaml-interp ocaml-base-nox ocaml-nox
}

install_opam2 () {
    case $TRAVIS_OS_NAME in
        linux)
            case $TRAVIS_DIST in
                precise|trusty|xenial)
                    add_ppa ansible/bubblewrap ;;
            esac
            if [ "${INSTALL_LOCAL:=0}" = 0 ] ; then
                install_ocaml
            fi
            apt_install bubblewrap
            sudo wget https://github.com/ocaml/opam/releases/download/$OPAM_VERSION/opam-$OPAM_VERSION-x86_64-linux -O /usr/local/bin/opam
            sudo chmod +x /usr/local/bin/opam ;;
        osx)
            if [ "${INSTALL_LOCAL:=0}" = 0 ] ; then
                brew install ocaml
            fi
            sudo curl -fsSL https://github.com/ocaml/opam/releases/download/$OPAM_VERSION/opam-$OPAM_VERSION-x86_64-macos -o /usr/local/bin/opam
            sudo chmod +x /usr/local/bin/opam ;;
    esac
}

install_ppa () {
  add_ppa $1
  if [ "${INSTALL_LOCAL:=0}" = 0 ] ; then
    sudo apt-get -qq update
    APT_UPDATED=1
    apt_install \
       "$(full_apt_version ocaml $SYS_OCAML_VERSION)" \
       "$(full_apt_version ocaml-base $SYS_OCAML_VERSION)" \
       "$(full_apt_version ocaml-native-compilers $SYS_OCAML_VERSION)" \
       "$(full_apt_version ocaml-compiler-libs $SYS_OCAML_VERSION)" \
       "$(full_apt_version ocaml-interp $SYS_OCAML_VERSION)" \
       "$(full_apt_version ocaml-base-nox $SYS_OCAML_VERSION)" \
       "$(full_apt_version ocaml-nox $SYS_OCAML_VERSION)"
  fi
  apt_install opam
}

install_on_linux () {
	install_opam2

  XENIAL="deb mirror://mirrors.ubuntu.com/mirrors.txt xenial main restricted universe"

  if [ "$UPDATE_GCC_BINUTILS" != "0" ] ; then
    echo "installing a recent gcc and binutils (mainly to get mirage-entropy-xen working!)"
    sudo add-apt-repository "${XENIAL}"
    sudo add-apt-repository --yes ppa:ubuntu-toolchain-r/test
    sudo apt-get -qq update
    sudo apt-get install -y gcc-5
    sudo update-alternatives --install /usr/bin/gcc gcc /usr/bin/gcc-5 90
    sudo add-apt-repository -r "${XENIAL}"
  fi

  if [ "$UBUNTU_XENIAL" != "0" ] ; then
    echo "Adding Ubuntu Xenial mirrors"
    sudo add-apt-repository "${XENIAL}"
    sudo apt-get -qq update
    APT_UPDATED=1
  fi

  if [ "${INSTALL_LOCAL:=0}" != 0 ] ; then
    echo -en "travis_fold:start:build.ocaml\r"
    echo "Building a local OCaml; this may take a few minutes..."
    wget "http://caml.inria.fr/pub/distrib/ocaml-${OCAML_FULL_VERSION%.*}/ocaml-$OCAML_FULL_VERSION.tar.gz"
    tar -xzf "ocaml-$OCAML_FULL_VERSION.tar.gz"
    cd "ocaml-$OCAML_FULL_VERSION"
    ./configure -prefix /usr/local ${OCAML_CONFIGURE_ARGS:=--with-debug-runtime}
    make world.opt
    sudo make install
    cd ..
    echo -en "travis_fold:end:build.ocaml\r"
  fi
}

install_on_osx () {
  case $INSTALL_XQUARTZ in
      true)
        curl -OL "http://xquartz.macosforge.org/downloads/SL/XQuartz-2.7.6.dmg"
        sudo hdiutil attach XQuartz-2.7.6.dmg
        sudo installer -verbose -pkg /Volumes/XQuartz-2.7.6/XQuartz.pkg -target /
        ;;
  esac
	install_opam2
}

case $TRAVIS_OS_NAME in
    osx) install_on_osx ;;
    linux) install_on_linux ;;
esac

ocaml_package=ocaml-base-compiler
if [ "$OCAML_BETA" = "enable" ]; then
    ocaml_package=ocaml-variants
fi

export OPAMYES=1

OPAM_SWITCH=$OCAML_VERSION

case $OPAM_INIT in
  true)
      opam init -a --bare "$BASE_REMOTE"
			opam switch create "$OPAM_SWITCH" --repositories=multicore=git+https://github.com/ocamllabs/multicore-opam.git,default
      eval $(opam config env)
      ;;
esac

echo OCAML_VERSION=$OCAML_VERSION >  .travis-ocaml.env
echo OPAM_SWITCH=$OPAM_SWITCH     >> .travis-ocaml.env

if [ -x "$(command -v ocaml)" ]; then
    ocaml -version
else
    echo "OCaml is not yet installed"
fi

opam --version
opam --git-version
