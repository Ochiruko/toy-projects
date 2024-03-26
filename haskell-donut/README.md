# haskell-donut
This project uses Gloss, a graphics library that depends on GLUT. For some reason, haskell does not install this by default, so you will need to install GLUT yourself to get this program to run. To do so:

## Linux
### Arch
```
 sudo pacman -S freeglut
```
### Debian
...
### Ubuntu
...
## Windows
https://github.com/haskell-opengl/GLUT/pull/23
Windows is a bit tricky. The PS script below is your best bet.

~~~
# URL of the ZIP file
$url = 'https://www.transmissionzero.co.uk/files/software/development/GLUT/freeglut-MinGW-3.0.0-1.mp.zip'

# Local path to save the downloaded ZIP file
$localZipPath = '.\freeglut.zip'

# Destination folder to extract the contents of the ZIP file
$destinationFolder = '.\'

# Download the ZIP file from the URL
Invoke-WebRequest -Uri $url -OutFile $localZipPath

# Extract the ZIP file
Expand-Archive -LiteralPath $localZipPath -DestinationPath $destinationFolder -Force

# Path to the specific file you want to work with
$specificFile = 'freeglut\bin\x64\freeglut.dll' # Replace with the actual file name
$extractedFilePath = Join-Path -Path $destinationFolder -ChildPath $specificFile

# Check if the file exists and then perform further operations
if (Test-Path -Path $extractedFilePath) {
    Write-Host "File extracted successfully: $extractedFilePath"
    # Add further actions here if needed
} else {
    Write-Host "File not found in the archive: $specificFile"
}
~~~
## MacOS
...
