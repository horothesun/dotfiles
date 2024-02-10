#!/bin/bash

HOME_TMP_FOLDER="${HOME}/tmp"
FACETIMEHD_FIRMWARE_DIR="facetimehd-firmware"
FACETIMEHD_MODULE_DIR="bcwc_pcie"

mkdir "${HOME_TMP_FOLDER}"

# build facetimehd drivers
(
  cd "${HOME_TMP_FOLDER}"
  git clone "https://github.com/patjak/${FACETIMEHD_FIRMWARE_DIR}.git"
  cd "${FACETIMEHD_FIRMWARE_DIR}"
  make
  sudo make install
)

# build and load module
(
  cd "${HOME_TMP_FOLDER}"
  git clone "https://github.com/patjak/${FACETIMEHD_MODULE_DIR}.git"
  cd "${FACETIMEHD_MODULE_DIR}"
  make
  sudo make install
  sudo depmod
  sudo modprobe -r bdc_pci
  sudo modprobe -r facetimehd
  sudo modprobe facetimehd
)

# cleanup
rm -fr "${HOME_TMP_FOLDER}/${FACETIMEHD_FIRMWARE_DIR}"
rm -fr "${HOME_TMP_FOLDER}/${FACETIMEHD_MODULE_DIR}"

echo
echo "!!! Test the FaceTime webcam with guvcview or skypeforlinux !!!"
echo "!!! A SYSTEM REBOOT MIGHT BE REQUIRED !!!"
echo
