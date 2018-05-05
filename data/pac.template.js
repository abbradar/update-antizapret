// Required for Mac OS X
if(typeof convert_addr === 'undefined') {
  function convert_addr(ipchars) {
    var bytes = ipchars.split('.');
    return ((bytes[0] & 0xff) << 24) |
      ((bytes[1] & 0xff) << 16) |
      ((bytes[2] & 0xff) <<  8) |
      (bytes[3] & 0xff);
  }
}

function arraysToBuckets(hashmap) {
  for (var key in hashmap) {
    var array = hashmap[key];
    var bucket = {};
    for (var i = 0; i < array.length; ++i) {
      var value = array[i];
      if (value instanceof Array) {
        for (var j = value[0]; j <= value[1]; ++j) {
          bucket[j] = null;
        }
      } else {
        bucket[value] = null;
      }
    }
    hashmap[key] = bucket;
  }
}

arraysToBuckets(ADDRESSES);
arraysToBuckets(SUBNETS);

function FindProxyForURL(url, host) {
  var ipAddr = convert_addr(dnsResolve(host));

  var bucket = ADDRESSES[ipAddr >>> SPLIT_SHIFT];
  if (bucket !== undefined) {
    if ((ipAddr & SPLIT_MASK) in bucket) {
      return PROXY;
    }
  }

  for (var mlen in SUBNETS) {
    var subnets = SUBNETS[mlen];
    if ((ipAddr >>> (32 - mlen)) in subnets) {
      return PROXY;
    }
  }

  return "DIRECT";
}
