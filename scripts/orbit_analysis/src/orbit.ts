// Get all members from the Orbit API
export async function getMembers(token: string): Promise<Member[]> {
  const yesterday = Date.now() - 1000 * 60 * 60 * 24;
  try {
    const fileinfo = await Deno.stat("members.json");
    if (fileinfo.mtime && fileinfo.mtime.getTime() > yesterday) {
      return JSON.parse(await Deno.readTextFile("members.json"));
    }
    console.log("members.json outdated, fetching from API");
  } catch (_error) {
    console.log("members.json not found, fetching from API");
  }

  const members = await getMembersFromApi(token);

  console.log("Writing members.json");
  await Deno.writeTextFile("members.json", JSON.stringify(members));

  return members;
}

async function getMembersFromApi(token: string): Promise<Member[]> {
  const members: Member[] = [];
  let next: string | undefined =
    "https://app.orbit.love/gleam/members.json?items=100";

  while (next) {
    console.log(`Fetching ${next}`);
    const response: MembersApiResponse = await get(next, token);
    members.push(...response.data);
    next = response.links.next;
  }

  return members;
}

async function get<T>(url: string, token: string): Promise<T> {
  const response = await fetch(url, {
    headers: { Authorization: `Bearer ${token}` },
  });
  if (!response.ok) throw new Error(response.statusText);
  return response.json();
}

export interface MembersApiResponse {
  data: Member[];
  links: {
    first?: string;
    prev?: string;
    next?: string;
  };
}

export interface Member {
  id: string;
  type: string;
  attributes: {
    activities_count: number;
    activities_score: number;
    avatar_url: string;
    bio: string;
    birthday?: Date;
    company: string;
    title: string;
    created_at: Date;
    deleted_at?: Date;
    first_activity_occurred_at: Date;
    last_activity_occurred_at: Date;
    location: string;
    name: string;
    pronouns?: string;
    reach?: number;
    shipping_address?: string;
    slug: string;
    source: string;
    tag_list: string[];
    tags: string[];
    teammate: boolean;
    tshirt?: string;
    updated_at: Date;
    merged_at?: Date;
    url: string;
    orbit_url: string;
    created: boolean;
    id: string;
    orbit_level?: number;
    love: string;
    twitter: string;
    github: string;
    discourse?: string;
    email: string;
    devto?: string;
    linkedin?: string;
    discord: string;
    github_followers?: number;
    twitter_followers?: number;
    topics: string[];
    languages: string[];
  };
  relationships: {
    identities: {
      data: {
        id: string;
        type: string;
      }[];
    };
  };
}
