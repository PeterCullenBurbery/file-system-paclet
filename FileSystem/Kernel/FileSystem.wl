(* ::Package:: *)

BeginPackage["PeterBurbery`FileSystem`"];

(* Declare your packages public symbols here. *)

DirectorySize;
BrowserOpen;

Begin["`Private`"];

(* Define your public and private symbols here. *)

DirectorySize[]:=DirectorySize[Directory[]];
DirectorySize[dir_ /; DirectoryQ[dir]] := Quantity[Internal`DirectoryByteCount[dir], "Bytes"]
(*BrowserOpen*)

BrowserOpen[url_String] /; $OperatingSystem =!= "Windows" := Message[BrowserOpen::notsupported, $OperatingSystem]
 
BrowserOpen[url_] := Module[{keys, exe}, keys = Association[Developer`ReadRegistryKeyValues["HKEY_CURRENT_USER\\SOFTWARE\\Microsoft\\Windows\\Shell\\Associations\\URLAssociations\\https\\UserChoice"]]; keys = Association[Developer`ReadRegistryKeyValues[StringJoin["HKEY_CLASSES_ROOT\\", keys["ProgId"], "\\Application"]]]; exe = First[StringSplit[keys["ApplicationIcon"], ","]]; RunProcess[{exe, url}]]

BrowserOpen::notsupported = "$OperatingSystem \"``\" is not supported.";

End[]; (* End `Private` *)

EndPackage[];
