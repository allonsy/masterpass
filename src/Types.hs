module Types where


data AlgorithmVersion =
  Version0 |
  Version1 |
  Version2 |
  Version3

currentVersion :: AlgorithmVersion
currentVersion = Version3


data SiteType =
  Maximum |
  Long    |
  Medium  |
  Basic   |
  Short   |
  PIN     |
  Name    |
  Phrase

data SiteVariant =
  Password |
  Login    |
  Answer
