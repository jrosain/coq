#!/usr/bin/env bash

ci_dir="$(dirname "$0")"
. "${ci_dir}/ci-common.sh"

git_download mczify

( cd "${CI_BUILD_DIR}/mczify" && make )
