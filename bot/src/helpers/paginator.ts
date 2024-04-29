import {
  APIEmbed,
  ButtonStyle,
  CommandInteraction,
  ComponentType,
  InteractionReplyOptions,
  Message,
} from "discord.js";
import { wrapMessageComponentCollector } from "./observable";
import * as rx from "rxjs";

export interface PageButtonOptions {
  /**
   * The style of the button.
   */
  style?: ButtonStyle.Primary | ButtonStyle.Secondary | ButtonStyle.Success | ButtonStyle.Danger;

  /**
   * The text to be displayed on the last button (Defaults to 'Last').
   */
  lastLabel?: string;

  /**
   * The text to be displayed on the next button (Defaults to 'Next').
   */
  nextLabel?: string;

  /**
   * The text to be displayed on the next button (Defaults to 'Next').
   */
  stopLabel?: string;

  /**
   * The text to be displayed on the previous button. (Defaults to 'Previous').
   */
  previousLabel?: string;

  /**
   * The text to be displayed on the first button. (Defaults to 'First').
   */
  firstLabel?: string;

  /**
   * The message to be alongside the paginated embeds.
   */
  content?: string;

  /**
   * Whether or not to show the current page in the footer of each embed (Defaults to being shown).
   */
  showPagePosition?: boolean;

  /**
   * How long the paginator should run for in ms. (Default is 30min)
   */
  time?: number;

  /**
   * The label that displays in the page position footer.
   */
  pageLabel?: string;
}

type PaginationProps = {
  interaction: CommandInteraction;
  embeds: APIEmbed[];
  options?: PageButtonOptions;
};
/**
 * Sends a paginated message from the given embeds.
 * @param interaction The interaction to reply to.
 * @param embeds The array of embeds to use.
 */
export async function sendPaginatedEmbeds({
  interaction,
  embeds,
  options: {
    style = ButtonStyle.Primary,
    firstLabel = "First",
    previousLabel = "Previous",
    stopLabel = "Stop",
    nextLabel = "Next",
    lastLabel = "Last",
    content,
    showPagePosition = true,
    // Default to half an hour.
    time = 1800000,
    pageLabel = "Embed page",
  } = {},
}: PaginationProps): Promise<Message> {
  let currentPage = 0;

  // Precheck
  if (interaction.replied)
    throw new Error("Cannot paginate when interaction is already replied to.");

  const generateOptionsForPage = (page: number): InteractionReplyOptions => {
    const currentEmbed = embeds[page];

    if (!currentEmbed) throw new Error("Embed page number out of bounds");

    return {
      embeds: [
        {
          ...currentEmbed,
          ...(showPagePosition
            ? {
                footer: {
                  text: currentEmbed.footer?.text
                    ? `${currentEmbed.footer?.text} (${pageLabel} ${
                        currentPage + 1
                      } of ${embeds.length})`
                    : `${pageLabel} ${currentPage + 1} of ${embeds.length}`,
                },
              }
            : {}),
        },
      ],
      components: [
        {
          type: ComponentType.ActionRow,
          components: [
            {
              type: ComponentType.Button,
              customId: "firstButton",
              label: firstLabel,
              style,
              disabled: page === 0,
            },
            {
              type: ComponentType.Button,
              customId: "previousButton",
              label: previousLabel,
              style,
              disabled: page === 0,
            },
            {
              type: ComponentType.Button,
              customId: "stopButton",
              label: stopLabel,
              style,
            },
            {
              type: ComponentType.Button,
              customId: "nextButton",
              label: nextLabel,
              style,
              disabled: page === embeds.length - 1,
            },
            {
              type: ComponentType.Button,
              customId: "lastButton",
              label: lastLabel,
              style,
              disabled: page === embeds.length - 1,
            },
          ],
        },
      ],
    };
  };

  const messageOptions = generateOptionsForPage(0);

  const message = (
    interaction.deferred
      ? await interaction.editReply({
          ...messageOptions,
          content,
        })
      : await interaction.reply({
          ...messageOptions,
          fetchReply: true,
          content,
        })
  ) as Message;

  const onEnd = async () => {
    if (!message.editable) {
      return;
    }

    await message.edit({ components: [] });
  };

  wrapMessageComponentCollector(
    message.createMessageComponentCollector({
      componentType: ComponentType.Button,
      time,
    })
  )
    .pipe(
      rx.concatMap((i) =>
        rx.from(
          (async () => {
            await i.deferUpdate();
            return i;
          })()
        )
      ),
      rx.filter((i) => i.isButton()),
      rx.takeWhile((i) => i.customId !== "stopButton", true)
    )
    .subscribe({
      next: async (collectInteraction) => {
        switch (collectInteraction.customId) {
          case "firstButton":
            currentPage = 0;
            break;
          case "previousButton":
            currentPage--;
            break;
          case "stopButton":
            await onEnd();
            return;
          case "nextButton":
            currentPage++;
            break;
          case "lastButton":
            currentPage = embeds.length - 1;
            break;
        }
        const replyOptions = generateOptionsForPage(currentPage);
        await collectInteraction.editReply(replyOptions);
      },
      complete: onEnd,
    });

  return message;
}
