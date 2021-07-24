import { Elm } from "./Main.elm";
import 'regenerator-runtime/runtime';

const app = Elm.Main.init({ node: document.getElementById("main") });

(async () => {
    const webt = await WabtModule();
    // elm -> js
    app.ports.compile.subscribe(async (source) => {
        try {
	//     const module = webt.parseWat('test.wat', source, {});
        //     const binaryOutput = module.toBinary({log: true, write_debug_names:true});
	    // js -> elm
	//     app.ports.receiveWasm.send(binaryOutput.log);
 	    const bufferSource = new Uint8Array(source);
	    console.log(bufferSource);
	    const results = await WebAssembly.instantiate(bufferSource, {});
	    console.log(results);
	//     console.log(results.instance.exports.add());
	} catch(e) {
	    app.ports.receiveWasm.send("compile error");
	}
    });
})();