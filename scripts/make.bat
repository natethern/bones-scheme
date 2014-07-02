nasm -f win64 bones-x86_64-windows.s -o bones.obj
link bones.obj libcmt.lib /out:bones.exe
