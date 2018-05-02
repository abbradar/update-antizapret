for (var key in ADDRESSES) {
  var array = ADDRESSES[key];
  var dict = {};
  for (var i = 0; i < array.length; ++i) {
    dict[array[i]] = null;
  }
  ADDRESSES[key] = dict;
}

function FindProxyForURL(url, host) {
  var ipAddr = convert_addr(dnsResolve(host));

  var chunk = ADDRESSES[ipAddr >>> SPLIT_SHIFT];
  if (chunk !== undefined) {
    if ((ipAddr & SPLIT_MASK) in chunk) {
      return PROXY;
    }
  }

  for (var i = 0; i < SUBNETS.length; i++) {
    var subnet = SUBNETS[i];
    if ((ipAddr & subnet[1]) === subnet[0]) {
      return PROXY;
    }
  }

  return "DIRECT";
}
