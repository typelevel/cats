#!/bin/bash

# Build Overview:
# The overall build is split into a number of parts
# 1. The build for coverage is performed. This:
#   a. First enables the coverage processing, and then
#   b. Builds and tests for the JVM using the validateJVM target, and then
#   c. Produces the coverage report, and then
#   d. Clean is run (as part of coverageReport), to clear down the built artifacts
# 2. The scala js build is executed, compiling the application and testing it for scala js.
# 3. The validateJVM target is executed again, due to the fact that producing coverage with the
#    code coverage tool causes the byte code to be instrumented/modified to record the coverage
#    metrics when the tests are executing. This causes the full JVM build to be run a second time.
#    This step is omitted in PR builds to speed them up. In the off-chance that the coverage tool
#    changes the behavior of code by instrumenting it, the master build should be able to catch
#    the issue before a release happens.

# Example setting to use at command line for testing:
# export TRAVIS_SCALA_VERSION=2.10.5;export TRAVIS_PULL_REQUEST="false";export TRAVIS_BRANCH="master"

sbt_cmd="sbt ++$TRAVIS_SCALA_VERSION"
coverage_cmd="$sbt_cmd coverage validateJVM coverageReport && codecov"

if [[ $TRAVIS_PULL_REQUEST == "false" && $TRAVIS_BRANCH == "master" && $(cat version.sbt) =~ "-SNAPSHOT" ]]; then
  eval "$coverage_cmd && $sbt_cmd validateJS validateJVM publish gitSnapshots publish"
  # temporarily disable to stabilize travis
  #if [[ $TRAVIS_SCALA_VERSION = "2.11.8" ]]; then
  #  export publish_cmd="$publish_cmd ghpagesPushSite"
  #fi
else
  eval "$coverage_cmd && $sbt_cmd validateJS publishLocal"
fi
