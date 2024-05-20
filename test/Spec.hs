import Test.HUnit
import HaskellYaml

import qualified System.Exit as Exit

testNull :: Test
testNull = TestCase (do
  assertEqual "should parse null" (Just ("", YamlScalar YamlNull)) (runParser yamlP "null")
  assertEqual "should parse Null" (Just ("", YamlScalar YamlNull)) (runParser yamlP "Null")
  assertEqual "should parse NULL" (Just ("", YamlScalar YamlNull)) (runParser yamlP "NULL")
  assertEqual "should parse ~" (Just ("", YamlScalar YamlNull)) (runParser yamlP "~"))
  -- TODO: assertEqual "should parse empty" (Just ("", YamlNull)) (runParser yamlP "")

testBool :: Test
testBool = TestCase (do
  assertEqual "should parse bool true" (Just ("", YamlScalar (YamlBool True))) (runParser yamlP "true")
  assertEqual "should parse bool True" (Just ("", YamlScalar (YamlBool True))) (runParser yamlP "True")
  assertEqual "should parse bool TRUE" (Just ("", YamlScalar (YamlBool True))) (runParser yamlP "TRUE")
  assertEqual "should parse bool false" (Just ("", YamlScalar (YamlBool False))) (runParser yamlP "false")
  assertEqual "should parse bool False" (Just ("", YamlScalar (YamlBool False))) (runParser yamlP "False")
  assertEqual "should parse bool FALSE" (Just ("", YamlScalar (YamlBool False))) (runParser yamlP "FALSE"))

testInt :: Test
testInt = TestCase (do
  assertEqual "should parse int" (Just ("", YamlScalar (YamlInt 685230))) (runParser yamlP "685230")
  assertEqual "should parse int positive" (Just ("", YamlScalar (YamlInt 685230))) (runParser yamlP "+685230")
  assertEqual "should parse int positive" (Just ("", YamlScalar (YamlInt (-685230)))) (runParser yamlP "-685230")
  assertEqual "should parse int decimal" (Just ("", YamlScalar (YamlInt 685230))) (runParser yamlP "685_230")
  assertEqual "should parse int decimal long" (Just ("", YamlScalar (YamlInt 685230))) (runParser yamlP "685____230")
  assertEqual "should parse int decimal start" (Just ("", YamlScalar (YamlInt 685230))) (runParser yamlP "__685230")
  assertEqual "should parse int decimal end" (Just ("", YamlScalar (YamlInt 685230))) (runParser yamlP "685230__")
  assertEqual "should parse int decimal start and end" (Just ("", YamlScalar (YamlInt 685230))) (runParser yamlP "__685230__")
  assertEqual "should parse int decimal positive" (Just ("", YamlScalar (YamlInt 685230))) (runParser yamlP "+685_230")
  assertEqual "should parse int decimal negative" (Just ("", YamlScalar (YamlInt (-685230)))) (runParser yamlP "-685_230"))
  -- TODO: assertEqual "should parse int octal" (Just ("", YamlScalar (YamlInt 685230))) (runParser yamlP "0o2472256")
  -- TODO: assertEqual "should parse int octal positive" (Just ("", YamlScalar (YamlInt 685230))) (runParser yamlP "+0o2472256")
  -- TODO: assertEqual "should parse int octal negative" (Just ("", YamlScalar (YamlInt (-685230)))) (runParser yamlP "-0o2472256")
  -- TODO: assertEqual "should parse int hex" (Just ("", YamlScalar (YamlInt 685230))) (runParser yamlP "0x0A74AE")
  -- TODO: assertEqual "should parse int hex positive" (Just ("", YamlScalar (YamlInt 685230))) (runParser yamlP "+0x0A74AE")
  -- TODO: assertEqual "should parse int hex negative" (Just ("", YamlScalar (YamlInt (-685230)))) (runParser yamlP "-0x0A74AE")
  -- TODO: assertEqual "should parse int hex decimal" (Just ("", YamlScalar (YamlInt 685230))) (runParser yamlP "0x_0A_74_AE")
  -- TODO: assertEqual "should parse int hex decimal positive" (Just ("", YamlScalar (YamlInt 685230))) (runParser yamlP "+0x_0A_74_AE")
  -- TODO: assertEqual "should parse int hex decimal negative" (Just ("", YamlScalar (YamlInt (-685230)))) (runParser yamlP "-0x_0A_74_AE")
  -- TODO: assertEqual "should parse int binary" (Just ("", YamlScalar (YamlInt 685230))) (runParser yamlP "0b10100111010010101110")
  -- TODO: assertEqual "should parse int binary positive" (Just ("", YamlScalar (YamlInt 685230))) (runParser yamlP "+0b10100111010010101110")
  -- TODO: assertEqual "should parse int binary negative" (Just ("", YamlScalar (YamlInt (-685230)))) (runParser yamlP "-0b10100111010010101110")
  -- TODO: assertEqual "should parse int binary decimal" (Just ("", YamlScalar (YamlInt 685230))) (runParser yamlP "0b1010_0111_0100_1010_1110")
  -- TODO: assertEqual "should parse int binary decimal positive" (Just ("", YamlScalar (YamlInt 685230))) (runParser yamlP "+0b1010_0111_0100_1010_1110")
  -- TODO: assertEqual "should parse int binary decimal negative" (Just ("", YamlScalar (YamlInt (-685230)))) (runParser yamlP "-0b1010_0111_0100_1010_1110")

testFloat :: Test
testFloat = TestCase (do
  assertEqual "should parse float" (Just ("", YamlScalar (YamlFloat 685230.15))) (runParser yamlP "685230.15")
  assertEqual "should parse float positive" (Just ("", YamlScalar (YamlFloat 685230.15))) (runParser yamlP "+685230.15")
  assertEqual "should parse float negative" (Just ("", YamlScalar (YamlFloat (-685230.15)))) (runParser yamlP "-685230.15")
  assertEqual "should parse float decimal" (Just ("", YamlScalar (YamlFloat 685230.15))) (runParser yamlP "685_230.15")
  assertEqual "should parse float decimal positive" (Just ("", YamlScalar (YamlFloat 685230.15))) (runParser yamlP "+685_230.15")
  assertEqual "should parse float decimal negative" (Just ("", YamlScalar (YamlFloat (-685230.15)))) (runParser yamlP "-685_230.15")
  assertEqual "should parse float scientific notation positive " (Just ("", YamlScalar (YamlFloat 685230.15))) (runParser yamlP "6.8523015e+5")
  assertEqual "should parse float positive scientific notation positive" (Just ("", YamlScalar (YamlFloat 685230.15))) (runParser yamlP "+6.8523015e+5")
  assertEqual "should parse float negative scientific notation positive" (Just ("", YamlScalar (YamlFloat (-685230.15)))) (runParser yamlP "-6.8523015e+5")
  assertEqual "should parse float scientific notation negative " (Just ("", YamlScalar (YamlFloat 685230.15))) (runParser yamlP "68523015000.0e-5")
  assertEqual "should parse float positive scientific notation negative" (Just ("", YamlScalar (YamlFloat 685230.15))) (runParser yamlP "+68523015000.0e-5")
  assertEqual "should parse float negative scientific notation negative" (Just ("", YamlScalar (YamlFloat (-685230.15)))) (runParser yamlP "-68523015000.0e-5"))
  -- TODO: assertEqual "should parse float inf " (Just ("", YamlScalar (YamlFloat 685230.15))) (runParser yamlP ".inf")
  -- TODO: assertEqual "should parse float inf positive" (Just ("", YamlScalar (YamlFloat 685230.15))) (runParser yamlP "+.inf")
  -- TODO: assertEqual "should parse float inf negative" (Just ("", YamlScalar (YamlFloat (-685230.15)))) (runParser yamlP "-.inf")
  -- TODO: assertEqual "should parse float nan " (Just ("", YamlScalar (YamlFloat 685230.15))) (runParser yamlP ".nan")
  -- TODO: assertEqual "should parse float nan positive" (Just ("", YamlScalar (YamlFloat 685230.15))) (runParser yamlP "+.nan")
  -- TODO: assertEqual "should parse float nan negative" (Just ("", YamlScalar (YamlFloat (-685230.15)))) (runParser yamlP "-.nan")
  -- TODO: assertEqual "should parse float NaN " (Just ("", YamlScalar (YamlFloat 685230.15))) (runParser yamlP ".NaN")
  -- TODO: assertEqual "should parse float NaN positive" (Just ("", YamlScalar (YamlFloat 685230.15))) (runParser yamlP "+.NaN")
  -- TODO: assertEqual "should parse float NaN negative" (Just ("", YamlScalar (YamlFloat (-685230.15)))) (runParser yamlP "-.NaN")

tests :: Test
tests = TestList [
  TestLabel "testNull" testNull,
  TestLabel "bool" testBool,
  TestLabel "int" testInt,
  TestLabel "float" testFloat]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
