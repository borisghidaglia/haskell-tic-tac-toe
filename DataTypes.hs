module DataTypes (Player (..)) where


data Player = Cross | Circle

instance Show Player where
  show Cross = "X"
  show Circle = "O"
