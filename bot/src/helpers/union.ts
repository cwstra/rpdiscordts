type PickUnionMember<
  TagKey extends string,
  Union extends { [T in TagKey]: string },
  PickedTags extends Union[TagKey]
> = Extract<Union, { [T in TagKey]: PickedTags }>;

export function unionSwitch<
  TagKey extends string,
  Union extends { [T in TagKey]: string },
  Result
>(
  tagKey: TagKey,
  member: Union,
  mapping: {
    [T in Union[TagKey]]: (member: PickUnionMember<TagKey, Union, T>) => Result;
  }
): Result {
  // Cast is needed here to keep TS happy.
  // Should be fine, since member must be
  // part of the type matching its own tag.
  return mapping[member[tagKey]](member as any);
}
