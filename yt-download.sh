#!/bin/bash

set -eo pipefail

check_deps() {
    command -v yt-dlp >/dev/null 2>&1 || { echo "Error: yt-dlp is required" >&2; exit 1; }
    command -v jq >/dev/null 2>&1 || { echo "Error: jq is required" >&2; exit 1; }
}

get_video_data() {
    yt-dlp --playlist-end "$1" \
           --flat-playlist \
           --extractor-args youtubetab:approximate_date \
           --ignore-errors \
           -j "$2" \
           | jq -r 'select(.url | contains("/shorts/") | not) |
                   "[\(if .upload_date? then .upload_date[0:4]+"-"+.upload_date[4:6]+"-"+.upload_date[6:8] else "DATE-N/A" end)] [\(.id)] \(.title)\n   \(.url)\n"'
}

download_video() {
    yt-dlp -f "bestvideo[ext=mp4]+bestaudio[ext=m4a]/best[ext=mp4]/best" \
        --merge-output-format mp4 \
        --embed-thumbnail \
        --embed-metadata \
        --no-mtime \
        -o "${OUTPUT_DIR}/%(upload_date>%Y-%m-%d)s - %(title)s.%(ext)s" \
        "$1"
}

usage() {
    cat <<EOF
Usage: $0 [OPTIONS] (VIDEO_ID|@channel)

Options:
  -l, --list N    List N latest videos from channel (default: 3)
  -d, --download  Download the specified video (default action for video IDs)
  -h, --help      Show this help

Examples:
  $0 dQw4w9WgXcQ         # Download specific video
  $0 @TsodingDaily       # List 3 latest videos
  $0 -l 5 @TsodingDaily  # List 5 latest videos
EOF
    exit 1
}

# Defaults
LIST_COUNT=3
OUTPUT_DIR="${HOME}/Videos"
MODE="download"

# Parse arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        -l|--list)
            [[ $2 =~ ^[0-9]+$ ]] || { echo "Error: List count must be a number" >&2; usage; }
            LIST_COUNT="$2"
            MODE="list"
            shift 2
            ;;
        -d|--download) MODE="download"; shift ;;
        -h|--help) usage ;;
        @*)
            [ -z "$INPUT" ] || { echo "Error: Only one channel or video ID can be specified" >&2; usage; }
            INPUT="$1"
            shift
            ;;
        *)
            [ -z "$INPUT" ] || { echo "Error: Only one channel or video ID can be specified" >&2; usage; }
            INPUT="$1"
            shift
            ;;
    esac
done

[ -n "$INPUT" ] || { echo "Error: Must specify either video ID or @channel" >&2; usage; }

check_deps

if [[ "$INPUT" == @* ]]; then
    # Channel handling
    if [[ "$MODE" == "download" ]]; then
        echo "Error: Cannot download entire channel. Use -l to list videos first." >&2
        usage
    fi

    echo "Latest $LIST_COUNT videos from $INPUT (excluding Shorts):"
    get_video_data "$LIST_COUNT" "https://youtube.com/$INPUT"
else
    # Video ID handling
    if [[ "$MODE" == "list" ]]; then
        echo "Error: Cannot list single video. Use @channel for listing." >&2
        usage
    fi

    echo "Downloading video $INPUT..."
    download_video "https://youtu.be/$INPUT"
    echo "Download complete!"
fi
