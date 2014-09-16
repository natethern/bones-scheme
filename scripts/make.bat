nasm -f win64 bones-x86_64-windows.s -o bones.obj
link bones.obj libcmt.lib /out:bones.exe
bones si.scm -o si.s
nasm -f win64 si.s -o si.obj
link si.obj libcmt.lib /out:si.exe
