#!/bin/bash

SYS_NAME=$(uname)
REPO="https://github.com/kshimo69/dotfiles.git"
DIST_DIR="${HOME}/.dotfiles"

# functions

install_xcode() {
    if [ ! -e "$(xcode-select -p)" ]
    then
        xcode-select --install
        exit 1
    fi

    if [ ! -e "$(xcrun --show-sdk-path)/usr/include/zlib.h" ]
    then
        xcode-select --install
        exit 1
    fi
}

install_homebrew() {
    ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
}

install_requirement_packages_by_brew() {
    brew install git
    brew install ansible
}

install_requirement_packages_by_yum() {
    sudo yum update -y
    sudo yum install -y epel-release
    sudo yum install -y git
    sudo yum install -y --enablerepo=epel ansible
}

clone_dotfiles() {
    git clone ${REPO} ${DIST_DIR}
}

do_ansible() {
    pushd ${DIST_DIR}
    ansible-playbook ./playbooks/default/${1}.yml -vvv
    popd
}

setup_for_mac() {
    install_xcode
    install_homebrew
    install_requirement_packages_by_brew
    clone_dotfiles
    do_ansible mac
}

setup_for_rhel() {
    install_requirement_packages_by_yum
    clone_dotfiles
    # DEBUG
    DIST_DIR="/vagrant/"
    do_ansible rhel
}

# require setup
if [ "${SYS_NAME}" == "Darwin" ]
then
    echo "Platform: Mac"
    setup_for_mac
    exit 0
elif [ "${SYS_NAME}" == "Linux" ]
then
    which yum
    if [ $? -eq 0 ]
    then
        echo "Platform: Linux(yum)"
        setup_for_rhel
        exit 0
    else
        echo "This linux envitonment is not supported."
        exit 1
    fi
else
    echo "This platform is not supported."
    exit 1
fi
