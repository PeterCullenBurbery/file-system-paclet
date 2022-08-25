(* ::Package:: *)

BeginPackage["PeterBurbery`FileSystem`"];

(* Declare your packages public symbols here. *)

DirectorySize;
BrowserOpen;
ImageRead;
InternetArchiveData;

Begin["`Private`"];

(* Define your public and private symbols here. *)
ClearAll[DirectorySize,BrowserOpen,
ImageRead,InternetArchiveData]
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

InternetArchiveData["Search",query_String]:=Module[{template,json},
template=StringTemplate["https://archive.org/advancedsearch.php?q=`query`&output=json"];
json=Import[TemplateApply[template,<|"query"->query|>],"RawJSON"];
Map[#["identifier"]&,json["response","docs"]]
]

InternetArchiveData["Files",id_String]:=Module[{template,response,xml,ds},
template=StringTemplate["https://archive.org/download/`id`/`id`_files.xml"];
response=URLRead@TemplateApply[template,<|"id"->id|>];
xml=ImportString[response["Body"],"XML"];
ds=Dataset[
Cases[xml,
XMLElement["file",
{"name"->name_,"source"->source_},
meta:{XMLElement[_,{},{_}]..}]:>Join[
<|"name"->name,"source"->source|>,
Association[meta/.{XMLElement[key_,{},{value_}]:>(key->value)}]],\[Infinity]]]
]

InternetArchiveData["Import",id_String,file_String]:=Module[{template,response,xml,ds},
template=StringTemplate["https://archive.org/download/`id`/`file`"];
Import@TemplateApply[template,<|"id"->id,"file"->file|>]
]
End[]; (* End `Private` *)

EndPackage[];
