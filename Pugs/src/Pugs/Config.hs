{-# OPTIONS -fglasgow-exts #-}

{-
    *** NOTE ***
    DO NOT EDIT THIS FILE.
    This module is generated by util/PugsConfig.pm.
-}

{-|
    Pugs System Configuration.

>   Alive without breath;
>   as cold as death;
>   never thirsting, ever drinking;
>   clad in mail, never clinking.
-}


module Pugs.Config (
    config,
    getConfig
) where

import qualified Data.Map as Map
import qualified Pugs.Version

config :: Map.Map String String
config = Map.fromList
    [("archlib", "/usr/local/lib/perl6/darwin-2level"),
    ("archname", "darwin-2level"),
    ("bin", "/usr/local/bin"),
    ("cc", "i686-apple-darwin8-gcc-4.0.1 (GCC) 4.0.1 (Apple Computer, Inc. build 5367)"),
    ("embedded", "/usr/local/bin/perl5.9.5 noparrot nohaskell"),
    ("exe_ext", ""),
    ("file_sep", "/"),
    ("ghc", "/usr/local/bin/ghc 6.8.20070912 readline nohsplugins threads"),
    ("installarchlib", "/usr/local/lib/perl6/darwin-2level"),
    ("installbin", "/usr/local/bin"),
    ("installman1dir", "/usr/local/share/man/man1"),
    ("installman3dir", "/usr/local/share/man/man3"),
    ("installprivlib", "/usr/local/lib/perl6"),
    ("installscript", "/usr/local/bin"),
    ("installsitearch", "/usr/local/lib/perl6/site_perl/darwin-2level"),
    ("installsitebin", "/usr/local/bin"),
    ("installsitelib", "/usr/local/lib/perl6/site_perl"),
    ("installsiteman1dir", "/usr/local/share/man/man1"),
    ("installsiteman3dir", "/usr/local/share/man/man3"),
    ("osname", "darwin"),
    ("pager", "/usr/bin/less"),
    ("path_sep", ":"),
    ("perl5_path", "/usr/local/bin/perl5.9.5"),
    ("perl_compiler", "pugs"),
    ("perl_revision", "6"),
    ("perl_subversion", "0"),
    ("perl_version", "0"),
    ("prefix", "/usr/local"),
    ("privlib", "/usr/local/lib/perl6"),
    ("pugspath", "/usr/local/bin/pugs"),
    ("regex_engine", "default"),
    ("scriptdir", "/usr/local/bin"),
    ("sitearch", "/usr/local/lib/perl6/site_perl/darwin-2level"),
    ("sitebin", "/usr/local/bin"),
    ("sitelib", "/usr/local/lib/perl6/site_perl"),
    ("siteprefix", "/usr/local"),
    ("sitescript", "/usr/local/bin"),
    ("sourcedir", "/Users/audreyt/work/pugs"),
    ("uname", "Darwin 8.10.1 i386")
    ,("pugs_versnum", Pugs.Version.versnum)
    ,("pugs_version", Pugs.Version.version)
    ,("pugs_revision", Pugs.Version.revnum)
    ]

getConfig :: String -> String
getConfig key = Map.findWithDefault "" key config