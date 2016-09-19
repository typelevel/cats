#!/usr/bin/env bash
#

set -euo pipefail

cd "${TMPDIR:-/tmp}"
git clone -b topic/2-12 "https://github.com/BennyHill/sbt-scoverage.git"
( cd sbt-scoverage && sbt -sbt-version 0.13.13-M1 publishLocal)
rm -rf  sbt-scoverage
