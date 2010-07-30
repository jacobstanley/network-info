@echo off
cabal configure -ftest && cabal build && dist\build\test-network-info\test-network-info.exe
