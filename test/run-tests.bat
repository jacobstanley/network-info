@echo off
cabal configure && cabal build && .\dist\build\test-network-info\test-network-info.exe
