#!/bin/bash
# Copyright (C) InnoviData GmbH <http://www.innovidata.com>, 2011.
# Author: Holger Widmann <holger.widmann@innovidata.com>
# Version: 1.2.20110610
#
# License:
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License (LGPL) as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the LGPL for
# more details. You should have received a copy of the LGPL along with
# this program. If not, see <http://www.gnu.org/licenses/>.
#
# About:
# Installs a Truetype font family for use with LaTeX (pdftex).
# Font series bold is used for both semi-bold and extra-bold.
# The script tries to rename the fonts automatically according
# to the Karl-Berry-scheme, e.g. lfrr8t.ttf stands for Linotype
# Frutiger LT Roman with T1 font-encoding.
#
# Instructions:
# Sudo password is required to put the truetype fonts, the font metrics,
# the font definition and the font mapping into TEXMF.
# 1. Put the ttf files you want to use with LaTeX into a directory.
# 2. Put this script on your path or into the dir with the ttf files.
# 3. Edit TEXMF, FONTFOUNDRY, FONTNAME, FONTFAMILY, FONTENC, FONTDEFENC
#    and FONTENCFILE according to your font and desired encoding.
# 4. Execute this script.
# 5. Use
#   \renewcommand{\[rm|sf|tt]default}{FONTFAMILY}
#   \renewcommand{\familydefault}{\[rm|sf|tt]default}
# in preamble to change the font for the whole document.
#
# Other resources dealing with truetype fonts for LaTeX:
# http://c.caignaert.free.fr/ttf-english.html
# http://www.radamir.com/tex/ttf-tex.htm
# http://www.ctan.org/tex-archive/fonts/psfonts/w-a-schmidt
# http://www.wh10.tu-dresden.de/~lego/material/LaTeX_und_Fonts_Kirpal.pdf
# http://www.tex.ac.uk/ctan/support/installfont/installfont.pdf
# http://www.tex.ac.uk/tex-archive/info/Type1fonts/fontinstallationguide/fontinstallationguide.pdf
# http://www.dalug.org/fileadmin/veranstaltungen/Slides/truetype.pdf
# http://installfont.berlios.de
# http://fachschaft.physik.uni-greifswald.de/~stitch/ttf.html

TEXMF="/usr/local/share/texmf"
FONTFOUNDRY="linotype"
FONTNAME="frutiger"
FONTFAMILY="lfr"
FONTENC="8t"
FONTDEFENC="t1"
FONTENCFILE="T1-WGL4.enc"

FD="${TEXMF}/tex/latex/${FONTFOUNDRY}/${FONTNAME}"
MAP="${TEXMF}/fonts/map/dvips/${FONTNAME}"
TFM="${TEXMF}/fonts/tfm/${FONTFOUNDRY}/${FONTNAME}"
TTF="${TEXMF}/fonts/truetype/${FONTFOUNDRY}/${FONTNAME}"

# Check for ttf files in current dir.
if [ ! -e *.ttf ]; then
  echo "No Truetype fonts (*.ttf) found in current dir (${PWD})."
  exit
fi

# Check for necessary tools.
if [ $(which ttf2afm | wc -l) -lt 1 ]; then
  echo "ttf2afm is not available."
  exit
fi
if [ $(which ttf2tfm | wc -l) -lt 1 ]; then
  echo "ttf2tfm is not available."
  exit
fi
if [ $(which vptovf | wc -l) -lt 1 ]; then
  echo "vptovf is not available."
  exit
fi

echo "*** Creating directories (if neccessary)."
echo -n "${FD} "
if [ ! -d ${FD} ]; then
  sudo mkdir -p ${FD}
  echo "created."
else
  echo "exists."
fi
echo -n "${MAP} "
if [ ! -d ${MAP} ]; then
  sudo mkdir -p ${MAP}
  echo "created."
else
  echo "exists."
fi
echo -n "${TFM} "
if [ ! -d ${TFM} ]; then
  sudo mkdir -p ${TFM}
  echo "created."
else
  echo "exists."
fi
echo -n "${TTF} "
if [ ! -d ${TTF} ]; then
  sudo mkdir -p ${TTF}
  echo "created."
else
  echo "exists."
fi

echo "*** Deleting old files (.tfm, .ttf, .fd, .map)."
if [ -f ${FD}/${FONTDEFENC}${FONTFAMILY}.fd ]; then
  sudo rm -f ${FD}/${FONTDEFENC}${FONTFAMILY}.fd
fi
if [ -f ${MAP}/${FONTNAME}.map ]; then
  sudo rm -f ${MAP}/${FONTNAME}.map
fi
sudo rm -f ${TFM}/*
sudo rm -f ${TTF}/*

# Create a working directory.
TEMPDIR="/tmp/"`basename ${0}`"-"$(date +%Y%m%d-%H%M%S)
echo "*** Creating a working directory (${TEMPDIR})."
mkdir ${TEMPDIR}

# Rename the font files according to the Karl-Berry-Scheme.
echo "*** Renaming the truetype font files (.ttf) according to the Karl-Berry-scheme."
for FONTFILE in *.ttf; do
  FONTWEIGHT="r"
  WEIGHT=$(ttf2afm ${FONTFILE} 2>/dev/null | grep -ie "^weight[[:space:]]" | cut -d" " -f2)
  if [ ${WEIGHT} = "Normal" -o ${WEIGHT} = "normal" ]; then
    FONTWEIGHT="r"
  fi
  if [ ${WEIGHT} = "Bold" -o ${WEIGHT} = "bold" ]; then
    FONTWEIGHT="b"
  fi

  ANGLE=$(ttf2afm ${FONTFILE} 2>/dev/null | grep -ie "italicangle[[:space:]]" | cut -d" " -f2)
  if [ ${ANGLE} -lt 0 -o ${ANGLE} -gt 0 ]; then
    FONTANGLE="i"
  else
    FONTANGLE=""
  fi

  # Create an oblique font for every non-italic font file.
  if [ -z ${FONTANGLE} ]; then
    FONTFILENAME="${FONTFAMILY}${FONTWEIGHT}o"
    echo "${FONTFILE} ${TEMPDIR}/${FONTFILENAME}.ttf";
    cp "${FONTFILE}" ${TEMPDIR}/${FONTFILENAME}.ttf;
  fi

  FONTFILENAME="${FONTFAMILY}${FONTWEIGHT}${FONTANGLE}"
  echo "${FONTFILE} ${TEMPDIR}/${FONTFILENAME}.ttf";
  cp "${FONTFILE}" ${TEMPDIR}/${FONTFILENAME}.ttf;
done
cd ${TEMPDIR}

echo "*** Creating font mapping (.map)."
for FONTFILE in *.ttf; do
  FONT=${FONTFILE%%.*}
  NAME=$(ttf2afm ${FONTFILE} 2>/dev/null | grep -ie "^fontname[[:space:]]" | cut -d" " -f2)
  if [ ${FONT:(-1)} = "o" ]; then
    FONTSLANT="\" .167 SlantFont T1Encoding ReEncodeFont \" "
    SLANT="Oblique"
  fi
  echo "*** Creating TeX font metrics (.tfm) for ${FONTFILE} (${NAME}${SLANT}) (see ${FONT}.log)."
  ttf2tfm ${FONT}.ttf -q -T ${FONTENCFILE} -v ${FONT}${FONTENC}.vpl ${FONT}${FONTENC}.tfm 2>${FONT}.log
  vptovf ${FONT}${FONTENC}.vpl ${FONT}${FONTENC}.vf ${FONT}${FONTENC}.tfm
  FONTSLANT=""
  SLANT=""
  echo "${FONT}${FONTENC} ${NAME}${SLANT} ${FONTSLANT}<${FONT}.ttf <${FONTENCFILE}" >> ${FONTNAME}.map;
  rm ${FONT}${FONTENC}.vpl
  rm ${FONT}${FONTENC}.vf
done

echo "*** Creating font description (.fd)."
cat >${FONTDEFENC}${FONTFAMILY}.fd <<EOF
\ProvidesFile{${FONTDEFENC}${FONTFAMILY}.fd}[Font definitions for T1/${FONTFAMILY}.]

\DeclareFontFamily{T1}{${FONTFAMILY}}{}

\DeclareFontShape{T1}{${FONTFAMILY}}{m}{n}{<-> ${FONTFAMILY}r${FONTENC}}{}
\DeclareFontShape{T1}{${FONTFAMILY}}{m}{sc}{<-> ${FONTFAMILY}b${FONTENC}}{}
\DeclareFontShape{T1}{${FONTFAMILY}}{m}{sl}{<-> ${FONTFAMILY}ro${FONTENC}}{}
\DeclareFontShape{T1}{${FONTFAMILY}}{m}{it}{<-> ${FONTFAMILY}ri${FONTENC}}{}
\DeclareFontShape{T1}{${FONTFAMILY}}{b}{n}{<-> ${FONTFAMILY}b${FONTENC}}{}
\DeclareFontShape{T1}{${FONTFAMILY}}{b}{sc}{<-> ${FONTFAMILY}b${FONTENC}}{}
\DeclareFontShape{T1}{${FONTFAMILY}}{b}{sl}{<-> ${FONTFAMILY}bo${FONTENC}}{}
\DeclareFontShape{T1}{${FONTFAMILY}}{b}{it}{<-> ${FONTFAMILY}bi${FONTENC}}{}
\DeclareFontShape{T1}{${FONTFAMILY}}{sb}{n}{<->ssub * ${FONTFAMILY}/b/n}{}
\DeclareFontShape{T1}{${FONTFAMILY}}{sb}{sc}{<->ssub * ${FONTFAMILY}/b/sc}{}
\DeclareFontShape{T1}{${FONTFAMILY}}{sb}{sl}{<->ssub * ${FONTFAMILY}/b/sl}{}
\DeclareFontShape{T1}{${FONTFAMILY}}{sb}{it}{<->ssub * ${FONTFAMILY}/b/it}{}
\DeclareFontShape{T1}{${FONTFAMILY}}{bx}{n}{<->ssub * ${FONTFAMILY}/b/n}{}
\DeclareFontShape{T1}{${FONTFAMILY}}{bx}{sc}{<->ssub * ${FONTFAMILY}/b/sc}{}
\DeclareFontShape{T1}{${FONTFAMILY}}{bx}{sl}{<->ssub * ${FONTFAMILY}/b/sl}{}
\DeclareFontShape{T1}{${FONTFAMILY}}{bx}{it}{<->ssub * ${FONTFAMILY}/b/it}{}

\endinput
EOF

echo "*** Copying files (.ttf, .fd, .map, .tfm)."
sudo cp ${FONTDEFENC}${FONTFAMILY}.fd ${FD}
sudo cp ${FONTNAME}.map ${MAP}
sudo cp ${FONTFAMILY}*.tfm ${TFM}
sudo cp ${FONTFAMILY}*.ttf ${TTF}

echo "*** Updating TeX filename database."
sudo texhash ${TEXFM}

echo "*** Registering font mapping."
sudo updmap-sys --enable Map=${FONTNAME}.map
echo "*** Finished. The truetype font ${FONTNAME} is now available as ${FONTFAMILY} in LaTeX."
