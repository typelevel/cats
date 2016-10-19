#!/usr/bin/env bash
#

set -euo pipefail

cd "${TMPDIR:-/tmp}"
git clone -b tl-2-12 "https://github.com/BennyHill/scalac-scoverage-plugin.git"
( cd scalac-scoverage-plugin && sbt -sbt-version 0.13.13-M1 ++2.10.6 publishLocal ++2.11.8 publishLocal ++2.12.0-RC1 publishLocal )
rm -rf  scalac-scoverage-plugin

