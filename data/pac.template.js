function arraysToBuckets(hashmap) {
  for (var key in hashmap) {
    var array = hashmap[key];
    var bucket = {};
    for (var i = 0; i < array.length; ++i) {
      bucket[array[i]] = null;
    }
    ADDRESSES[key] = bucket;
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
    var ipPart = ipAddr >>> (32 - mlen);
    if ((ipAddr >>> (32 - mlen)) in subnets) {
      return PROXY;
    }
  }

  return "DIRECT";
}
