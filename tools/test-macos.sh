#!/bin/sh
set -eu

repository_dir=$(CDPATH='' cd -- "$(dirname -- "$0")/.." && pwd)
cd "$repository_dir"

if [ "$(uname -s)" != Darwin ] || [ "$(uname -m)" != arm64 ]; then
  echo "This wrapper requires macOS on Apple silicon." >&2
  exit 1
fi

for command in container curl lein; do
  if ! command -v "$command" >/dev/null 2>&1; then
    echo "Required command not found: $command." >&2
    exit 1
  fi
done

socket_path="$HOME/.socktainer/container.sock"
bridge_pid=
cleanup() {
  if [ -n "$bridge_pid" ]; then
    kill "$bridge_pid" 2>/dev/null || :
    wait "$bridge_pid" 2>/dev/null || :
  fi
}
trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

# shortcut: run one wrapper per user at a time; add locking if parallel runs are needed.
if ! curl -fsS --max-time 1 --unix-socket "$socket_path" http://localhost/_ping >/dev/null 2>&1; then
  bridge=$(command -v socktainer || printf '%s' "$repository_dir/target/bridge/socktainer")
  if [ ! -x "$bridge" ]; then
    echo "Socktainer is not on PATH or at target/bridge/socktainer." >&2
    exit 1
  fi
  mkdir -p target
  "$bridge" --no-docker-context >target/socktainer.log 2>&1 &
  bridge_pid=$!
  attempts=0
  until curl -fsS --max-time 1 --unix-socket "$socket_path" http://localhost/_ping >/dev/null 2>&1; do
    attempts=$((attempts + 1))
    if [ "$attempts" -ge 30 ] || ! kill -0 "$bridge_pid" 2>/dev/null; then
      echo "Socktainer did not start. Read target/socktainer.log." >&2
      exit 1
    fi
    sleep 1
  done
fi

# The bridge's pull-progress responses are incompatible with Docker Java.
container image pull --platform linux/arm64 docker.io/testcontainers/ryuk:0.14.0
container image pull --platform linux/arm64 docker.io/library/postgres:16

export DOCKER_HOST="unix://$socket_path"
export TESTCONTAINERS_DOCKER_SOCKET_OVERRIDE="$socket_path"
export TESTCONTAINERS_RYUK_DISABLED=false
# Match CI; a known timestamp conversion bug fails under Europe/Berlin.
export TZ=UTC
lein integration "$@"
