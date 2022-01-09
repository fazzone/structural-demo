const { contextBridge } = require('electron');
const fs = require('fs/promises')
const path = require('path')

contextBridge.exposeInMainWorld('my_electron_bridge', {
    spit: (function (p, c) {
        const rp = path.relative(".", p);
        const nested = rp && !rp.startsWith('..') && !path.isAbsolute(rp);
        if (!nested)
            return false;
        return fs.writeFile(rp, c);
    })
});
