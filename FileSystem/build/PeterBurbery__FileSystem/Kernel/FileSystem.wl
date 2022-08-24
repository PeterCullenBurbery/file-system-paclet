(* ::Package:: *)

BeginPackage["PeterBurbery`FileSystem`"];

(* Declare your packages public symbols here. *)

DirectorySize;
BrowserOpen;
ImageRead;
Begin["`Private`"];

(* Define your public and private symbols here. *)
ClearAll[DirectorySize,BrowserOpen,
ImageRead]
DirectorySize[]:=DirectorySize[Directory[]];
DirectorySize[dir_ /; DirectoryQ[dir]] := Quantity[Internal`DirectoryByteCount[dir], "Bytes"]
(*BrowserOpen*)

BrowserOpen[url_String] /; $OperatingSystem =!= "Windows" := Message[BrowserOpen::notsupported, $OperatingSystem]
 
BrowserOpen[url_] := Module[{keys, exe}, keys = Association[Developer`ReadRegistryKeyValues["HKEY_CURRENT_USER\\SOFTWARE\\Microsoft\\Windows\\Shell\\Associations\\URLAssociations\\https\\UserChoice"]]; keys = Association[Developer`ReadRegistryKeyValues[StringJoin["HKEY_CLASSES_ROOT\\", keys["ProgId"], "\\Application"]]]; exe = First[StringSplit[keys["ApplicationIcon"], ","]]; RunProcess[{exe, url}]]

BrowserOpen::notsupported = "$OperatingSystem \"``\" is not supported.";


Clear[ImageRead];
ImageRead[type_,File[file_String]]:=ImageRead[type,file];
ImageRead[type:("JPEG"|"PNG"|"TIFF"),file_String]:=Module[{function,image},
function=Switch[type,
"JPEG"|"JPG",Image`ImportExportDump`ImageReadJPEG,
"PNG",Image`ImportExportDump`ImageReadPNG,
"TIFF",Image`ImportExportDump`ImageReadTIFF];
image=First[function[file]];
If[ImageQ[image],image,$Failed]
];
ImageRead[___]:=$Failed
End[]; (* End `Private` *)

EndPackage[];
