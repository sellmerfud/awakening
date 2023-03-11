@ECHO off

SetLocal EnableDelayedExpansion

rem This script is used to run the scala implementation of the
rem solo AI for Labyrinth: The Awakening

rem Set the current working directory to the directory where this script is running.
rem This is important so that all of our relative path references work correctly.
pushd %~dp0
java -cp lib\loader.jar loader.Loader %*
popd
EndLocal
