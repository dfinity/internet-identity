{ lib, buildPythonPackage, fetchPypi, pytest, pytestcov, setuptools_scm }:

buildPythonPackage rec {
  pname = "cbor2";
  version = "5.2.0";

  src = fetchPypi {
    inherit pname version;
    sha256 = "1gwlgjl70vlv35cgkcw3cg7b5qsmws36hs4mmh0l9msgagjs4fm3";
  };

  nativeBuildInputs = [ setuptools_scm ];
  checkInputs = [ pytest pytestcov ];

  checkPhase = "pytest";

  meta = with lib; {
    description = "Pure Python CBOR (de)serializer with extensive tag support";
    homepage = "https://github.com/agronholm/cbor2";
    license = licenses.mit;
    maintainers = with maintainers; [ taneb ];
  };
}
