import gleam/time/timestamp
import teletext/event_log

const data =
  "1787580433072\tclimate/living room\t{\"battery\":100,\"humidity\":43.29,\"linkquality\":40,\"temperature\":22.41,\"voltage\":2900}
1787580776450\tplug/living room\t{\"child_lock\":\"UNLOCK\",\"current\":0,\"energy\":0,\"linkquality\":32,\"power\":0,\"state\":\"OFF\",\"voltage\":246}
1787580809467\tplug/living room\t{\"child_lock\":\"UNLOCK\",\"current\":0,\"energy\":0,\"linkquality\":25,\"power\":0,\"state\":\"OFF\",\"voltage\":245}
1787580809513\tplug/living room\t{\"child_lock\":\"UNLOCK\",\"current\":0,\"energy\":0,\"linkquality\":32,\"power\":0,\"state\":\"OFF\",\"voltage\":245}
1787581100596\tclimate/main bedroom\t{\"battery\":100,\"humidity\":47.14,\"linkquality\":36,\"temperature\":21.81,\"voltage\":2900}
1787581131762\tplug/living room\t{\"child_lock\":\"UNLOCK\",\"current\":0,\"energy\":0,\"linkquality\":36,\"power\":0,\"state\":\"OFF\",\"voltage\":245}
1787581268720\tclimate/second bedroom\t{\"battery\":100,\"humidity\":43.63,\"linkquality\":102,\"temperature\":21.78,\"voltage\":3000}
1787581409566\tplug/living room\t{\"child_lock\":\"UNLOCK\",\"current\":0,\"energy\":0,\"linkquality\":32,\"power\":0,\"state\":\"OFF\",\"voltage\":247}
1787581409608\tplug/living room\t{\"child_lock\":\"UNLOCK\",\"current\":0,\"energy\":0,\"linkquality\":29,\"power\":0,\"state\":\"OFF\",\"voltage\":247}
1787581434537\tplug/living room\t{\"child_lock\":\"UNLOCK\",\"current\":0,\"energy\":0,\"linkquality\":32,\"power\":0,\"state\":\"OFF\",\"voltage\":247}
1787581795028\tplug/living room\t{\"child_lock\":\"UNLOCK\",\"current\":0,\"energy\":0,\"linkquality\":36,\"power\":0,\"state\":\"OFF\",\"voltage\":247}
1787582009664\tplug/living room\t{\"child_lock\":\"UNLOCK\",\"current\":0,\"energy\":0,\"linkquality\":36,\"power\":0,\"state\":\"OFF\",\"voltage\":247}
1787582009708\tplug/living room\t{\"child_lock\":\"UNLOCK\",\"current\":0,\"energy\":0,\"linkquality\":36,\"power\":0,\"state\":\"OFF\",\"voltage\":247}
1787582136680\tplug/living room\t{\"child_lock\":\"UNLOCK\",\"current\":0,\"energy\":0,\"linkquality\":36,\"power\":0,\"state\":\"OFF\",\"voltage\":247}
1787582329812\tclimate/kitchen {\"battery\":100,\"humidity\":42.78,\"linkquality\":80,\"temperature\":22.13,\"voltage\":3000}
1787582360875\tclimate/kitchen {\"battery\":100,\"humidity\":42.46,\"linkquality\":76,\"temperature\":22.13,\"voltage\":3000}
1787582438048\tplug/living room\t{\"child_lock\":\"UNLOCK\",\"current\":0,\"energy\":0,\"linkquality\":29,\"power\":0,\"state\":\"OFF\",\"voltage\":247}
1787582609762\tplug/living room\t{\"child_lock\":\"UNLOCK\",\"current\":0,\"energy\":0,\"linkquality\":29,\"power\":0,\"state\":\"OFF\",\"voltage\":245}
1787582609805\tplug/living room\t{\"child_lock\":\"UNLOCK\",\"current\":0,\"energy\":0,\"linkquality\":36,\"power\":0,\"state\":\"OFF\",\"voltage\":245}
1787582774508\tplug/living room\t{\"child_lock\":\"UNLOCK\",\"current\":0,\"energy\":0,\"linkquality\":32,\"power\":0,\"state\":\"OFF\",\"voltage\":245}
1787583115198\tplug/living room\t{\"child_lock\":\"UNLOCK\",\"current\":0,\"energy\":0,\"linkquality\":36,\"power\":0,\"state\":\"OFF\",\"voltage\":245}
1787583209860\tplug/living room\t{\"child_lock\":\"UNLOCK\",\"current\":0,\"energy\":0,\"linkquality\":32,\"power\":0,\"state\":\"OFF\",\"voltage\":245}
1787583209904\tplug/living room\t{\"child_lock\":\"UNLOCK\",\"current\":0,\"energy\":0,\"linkquality\":32,\"power\":0,\"state\":\"OFF\",\"voltage\":245}
1787583248489\tclimate/second bedroom\t{\"battery\":100,\"humidity\":42.48,\"linkquality\":102,\"temperature\":21.78,\"voltage\":3000}
1787583252794\tclimate/living room\t{\"battery\":100,\"humidity\":42.07,\"linkquality\":21,\"temperature\":22.41,\"voltage\":2900}
1787583472383\tplug/living room\t{\"child_lock\":\"UNLOCK\",\"current\":0,\"energy\":0,\"linkquality\":32,\"power\":0,\"state\":\"OFF\",\"voltage\":245}
1787583566535\tclimate/kitchen {\"battery\":100,\"humidity\":41.83,\"linkquality\":80,\"temperature\":22.13,\"voltage\":3000}
1787583799188\tplug/living room\t{\"child_lock\":\"UNLOCK\",\"current\":0,\"energy\":0,\"linkquality\":32,\"power\":0,\"state\":\"OFF\",\"voltage\":245}
1787583809959\tplug/living room\t{\"child_lock\":\"UNLOCK\",\"current\":0,\"energy\":0,\"linkquality\":36,\"power\":0,\"state\":\"OFF\",\"voltage\":247}
1787583810002\tplug/living room\t{\"child_lock\":\"UNLOCK\",\"current\":0,\"energy\":0,\"linkquality\":32,\"power\":0,\"state\":\"OFF\",\"voltage\":247}
"

pub fn parse_empty_test() {
  let data = ""
  let assert Ok(events) = event_log.parse(data)
  assert events == []
}

pub fn parse_living_room_test() {
  let data =
    "1787580433072\tclimate/living room\t{\"battery\":100,\"humidity\":43.29,\"linkquality\":40,\"temperature\":22.41,\"voltage\":2900}"
  let assert Ok(events) = event_log.parse(data)
  assert events
    == [
      event_log.Event(
        time: timestamp.from_unix_seconds(1_787_580_433),
        topic: "climate/living room",
        payload: "{\"battery\":100,\"humidity\":43.29,\"linkquality\":40,\"temperature\":22.41,\"voltage\":2900}",
      ),
    ]
}
