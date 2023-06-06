TEST_FILE?=StableBuffer.test.mo

default:
	$(shell vessel bin)/moc $(shell vessel sources) -wasi-system-api -o Test.wasm ${TEST_FILE} && wasmtime Test.wasm
	rm -f Test.wasm
