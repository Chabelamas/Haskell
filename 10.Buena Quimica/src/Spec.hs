module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El producto Nafta, es aceptado por el sensor contieneSustancia al no tener funcionol " $ do
      contieneSustancia "funcionol" nafta `shouldBe` True
    it "El producto nafta es aceptado con la primera lista de sensores (sin las nuevas agregadas) " $ do
      habilitar nafta listaSensores `shouldBe` True
    it "Dados todos los productos del programa (incluye ejemplo), el unico producto que no era aceptado, pero con los nuevos sensores si, es ejemplo " $ do
     ahoraAceptados [nafta, bolitasNaftalina, paradigmetileno, escalonetina, ejemplo] listaSensores `shouldBe` [Producto {descripcion = "ejemplo", indicePeligrosidad = 100, componentes = ["funcionol","petroleo"]}]
