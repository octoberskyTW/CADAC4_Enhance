"%~dp0\setup.exe"

del "C:\Program Files\CADAC Studio\SWEEP++\sweep++.exe"
del "C:\Program Files\CADAC Studio\SWEEP\ellipse.exe"
del "C:\Program Files\CADAC Studio\SWEEP\sweep.exe"

copy "%~dp0\sweep++.exe" "C:\Program Files\CADAC Studio\SWEEP++\"
copy "%~dp0\ellipse.exe" "C:\Program Files\CADAC Studio\SWEEP\"
copy "%~dp0\sweep.exe" "C:\Program Files\CADAC Studio\SWEEP\"