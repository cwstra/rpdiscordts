import { CollectedInteraction, InteractionCollector } from "discord.js";
import { Observable } from "rxjs";

export function wrapMessageComponentCollector<T extends CollectedInteraction>(
  collector: InteractionCollector<T>
) {
  return new Observable<T>((subscriber) => {
    collector.on("collect", (interaction) => subscriber.next(interaction));
    collector.on("end", () => subscriber.complete());
  });
}
