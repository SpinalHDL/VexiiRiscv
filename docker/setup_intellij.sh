#!/bin/bash

echo "Installing IntelliJ IDEA..."

# We need root to install
[ $(id -u) != "0" ] && exec sudo "$0" "$@"

# define version (ultimate. change to 'C' for Community)
ed='C'

# Fetch the most recent community edition URL
VERSION=$(wget "https://www.jetbrains.com/intellij-repository/releases" -qO- | grep -P -o -m 1 "(?<=https://www.jetbrains.com/intellij-repository/releases/com/jetbrains/intellij/idea/BUILD/)[^/]+(?=/)")
URL="https://download.jetbrains.com/idea/ideaI$ed-$VERSION.tar.gz"

echo "URL: ${URL}"
echo "basename(url): $(basename ${URL})"

# Truncate filename
FILE=$(basename ${URL})

echo "File: ${FILE}"

# Download binary
wget -cO /tmp/${FILE} ${URL} --read-timeout=5 --tries=0

# Set directory name
DIR="${FILE%\.tar\.gz}"

# Untar file
mkdir -p /opt/${DIR}
tar -xvzf /tmp/${FILE} -C /opt/${DIR} --strip-components=1

# Grab executable folder
BIN="/opt/$DIR/bin"

# Add permissions to install directory
chmod 755 ${BIN}/idea.sh

# Set desktop shortcut path
DESK=/usr/share/applications/IDEA.desktop

# Add desktop shortcut                     
echo -e "[Desktop Entry]\nEncoding=UTF-8\nName=IntelliJ IDEA\nComment=IntelliJ IDEA\nExec=${BIN}/idea.sh\nIcon=${BIN}/idea.png\nTerminal=false\nStartupNotify=true\nType=Application" > ${DESK}
chmod 644 ${DESK}

echo "Done."  
