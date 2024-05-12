import { FormEvent, useEffect, useState } from "react";

interface Room {
  name: string;
  owner: string;
  notice: string;
  participants: string[];
}

export default function App() {
  const [nameInput, setNameInput] = useState("");
  const [roomNameInput, setRoomNameInput] = useState("");
  const [roomNoticeInput, setRoomNoticeInput] = useState("");

  const [name, setName] = useState("");

  const [rooms, setRooms] = useState<Room[]>([]);
  const [joinedRoom, setJoinedRoom] = useState<Room | undefined>();

  const [updateCounter, setUpdateCounter] = useState(0);

  useEffect(() => {
    const es = new EventSource("/api/event");

    es.onmessage = () => {
      setUpdateCounter((c) => c + 1);
    };

    return () => {
      es.close();
    };
  }, []);

  useEffect(() => {
    setName(localStorage.getItem("name") ?? "");
  }, []);

  useEffect(() => {
    const getRooms = async () => {
      const resp = await fetch("/api/rooms");
      const rooms: Room[] = await resp.json();
      setRooms(rooms);
    };

    getRooms();
  }, [updateCounter]);

  useEffect(() => {
    const getRoom = async () => {
      if (name) {
        const resp = await fetch(`/api/room?user=${name}`);
        const room: Room | undefined = await resp.json();
        setJoinedRoom(room);
      } else {
        setJoinedRoom(undefined);
      }
    };

    getRoom();
  }, [updateCounter, name]);

  const saveName = (e: FormEvent<HTMLFormElement>) => {
    e.preventDefault();

    if (nameInput) {
      localStorage.setItem("name", nameInput);
      setName(nameInput);
      setUpdateCounter((c) => c + 1);
    }
  };

  const createRoom = async (e: FormEvent<HTMLFormElement>) => {
    e.preventDefault();

    if (roomNameInput && roomNoticeInput) {
      await fetch("/api/room", {
        method: "POST",
        headers: {
          "content-type": "application/json",
        },
        body: JSON.stringify({
          name: roomNameInput,
          user: name,
          notice: roomNoticeInput,
        }),
      });
      setUpdateCounter((c) => c + 1);
    }
  };

  const joinRoom = async (roomName: string) => {
    await fetch("/api/room", {
      method: "PUT",
      headers: {
        "content-type": "application/json",
      },
      body: JSON.stringify({ name: roomName, user: name }),
    });
    setUpdateCounter((c) => c + 1);
  };

  const leaveRoom = async () => {
    await fetch("/api/room", {
      method: "DELETE",
      headers: {
        "content-type": "application/json",
      },
      body: JSON.stringify({ user: name }),
    });
    setUpdateCounter((c) => c + 1);
  };

  const leave = async () => {
    await leaveRoom();
    localStorage.setItem("name", "");
    setName("");
  };

  return (
    <div className="w-screen">
      <div className="navbar bg-base-200">
        <h1 className="text-xl font-bold px-8">/밥/</h1>
      </div>
      <div className="p-10 lg:w-2/3 mx-auto">
        {name ? (
          <div className="w-full">
            <div className="mb-4 flex items-center">
              <span className="text-lg font-bold mr-2">이름:</span>
              <span className="grow">{name}</span>
              <button
                className="btn btn-sm btn-warning"
                onClick={() => {
                  leave();
                }}
              >
                나가기
              </button>
            </div>
            {joinedRoom ? (
              <>
                <div className="divider">참여한 방</div>
                <div>
                  <div className="mb-2 flex items-center">
                    <span className="text-lg font-bold flex-1">
                      {joinedRoom.name}
                    </span>
                    {joinedRoom.owner}
                  </div>
                  <div className="mb-2 break-all">{joinedRoom.notice}</div>
                  <div className="mb-2">
                    <svg
                      xmlns="http://www.w3.org/2000/svg"
                      viewBox="0 0 24 24"
                      fill="currentColor"
                      className="w-6 h-6 inline mr-2"
                    >
                      <path
                        fillRule="evenodd"
                        d="M8.25 6.75a3.75 3.75 0 1 1 7.5 0 3.75 3.75 0 0 1-7.5 0ZM15.75 9.75a3 3 0 1 1 6 0 3 3 0 0 1-6 0ZM2.25 9.75a3 3 0 1 1 6 0 3 3 0 0 1-6 0ZM6.31 15.117A6.745 6.745 0 0 1 12 12a6.745 6.745 0 0 1 6.709 7.498.75.75 0 0 1-.372.568A12.696 12.696 0 0 1 12 21.75c-2.305 0-4.47-.612-6.337-1.684a.75.75 0 0 1-.372-.568 6.787 6.787 0 0 1 1.019-4.38Z"
                        clipRule="evenodd"
                      />
                      <path d="M5.082 14.254a8.287 8.287 0 0 0-1.308 5.135 9.687 9.687 0 0 1-1.764-.44l-.115-.04a.563.563 0 0 1-.373-.487l-.01-.121a3.75 3.75 0 0 1 3.57-4.047ZM20.226 19.389a8.287 8.287 0 0 0-1.308-5.135 3.75 3.75 0 0 1 3.57 4.047l-.01.121a.563.563 0 0 1-.373.486l-.115.04c-.567.2-1.156.349-1.764.441Z" />
                    </svg>
                    {joinedRoom.participants.length}
                  </div>
                  <ul className="mb-2">
                    {joinedRoom.participants.map((user) => (
                      <li key={user}>
                        <svg
                          xmlns="http://www.w3.org/2000/svg"
                          viewBox="0 0 24 24"
                          fill="currentColor"
                          className="w-4 h-4 inline mr-2"
                        >
                          <path
                            fillRule="evenodd"
                            d="M7.5 6a4.5 4.5 0 1 1 9 0 4.5 4.5 0 0 1-9 0ZM3.751 20.105a8.25 8.25 0 0 1 16.498 0 .75.75 0 0 1-.437.695A18.683 18.683 0 0 1 12 22.5c-2.786 0-5.433-.608-7.812-1.7a.75.75 0 0 1-.437-.695Z"
                            clipRule="evenodd"
                          />
                        </svg>
                        {user}
                      </li>
                    ))}
                  </ul>
                  <div className="flex justify-center">
                    <button
                      className="btn btn-error btn-circle w-16 h-16"
                      onClick={() => {
                        leaveRoom();
                      }}
                    >
                      도망
                    </button>
                  </div>
                </div>
              </>
            ) : (
              <>
                <div className="divider">방 만들기</div>
                <form className="flex" onSubmit={createRoom}>
                  <label className="input input-bordered flex items-center gap-2 mb-4 flex-[2] mr-2">
                    <span className="w-max">이름</span>
                    <input
                      type="text"
                      className="grow"
                      placeholder="심화왕 가실 분"
                      value={roomNameInput}
                      maxLength={30}
                      onChange={(e) => {
                        setRoomNameInput(e.target.value);
                      }}
                    />
                  </label>
                  <label className="input input-bordered flex items-center gap-2 mb-4 flex-[8] mr-2">
                    설명
                    <input
                      type="text"
                      className="grow"
                      placeholder="11시 정각 돌격"
                      value={roomNoticeInput}
                      onChange={(e) => {
                        setRoomNoticeInput(e.target.value);
                      }}
                    />
                  </label>
                  <input
                    type="submit"
                    className="btn btn-accent"
                    value="방 만들기"
                  />
                </form>
              </>
            )}
            <div className="divider">방 목록</div>
            <table className="table">
              <tbody>
                {rooms.map((room) => (
                  <tr
                    key={room.name}
                    className={
                      room.name === joinedRoom?.name ? "bg-base-200" : ""
                    }
                  >
                    <td className="font-bold">{room.name}</td>
                    <td className="mr-2">{room.owner}</td>
                    <td className="mr-2 break-all">{room.notice}</td>
                    <td className="mr-2 text-nowrap">
                      <svg
                        xmlns="http://www.w3.org/2000/svg"
                        viewBox="0 0 24 24"
                        fill="currentColor"
                        className="w-6 h-6 inline mr-2"
                      >
                        <path
                          fillRule="evenodd"
                          d="M8.25 6.75a3.75 3.75 0 1 1 7.5 0 3.75 3.75 0 0 1-7.5 0ZM15.75 9.75a3 3 0 1 1 6 0 3 3 0 0 1-6 0ZM2.25 9.75a3 3 0 1 1 6 0 3 3 0 0 1-6 0ZM6.31 15.117A6.745 6.745 0 0 1 12 12a6.745 6.745 0 0 1 6.709 7.498.75.75 0 0 1-.372.568A12.696 12.696 0 0 1 12 21.75c-2.305 0-4.47-.612-6.337-1.684a.75.75 0 0 1-.372-.568 6.787 6.787 0 0 1 1.019-4.38Z"
                          clipRule="evenodd"
                        />
                        <path d="M5.082 14.254a8.287 8.287 0 0 0-1.308 5.135 9.687 9.687 0 0 1-1.764-.44l-.115-.04a.563.563 0 0 1-.373-.487l-.01-.121a3.75 3.75 0 0 1 3.57-4.047ZM20.226 19.389a8.287 8.287 0 0 0-1.308-5.135 3.75 3.75 0 0 1 3.57 4.047l-.01.121a.563.563 0 0 1-.373.486l-.115.04c-.567.2-1.156.349-1.764.441Z" />
                      </svg>
                      {room.participants.length}
                    </td>
                    <td className="text-nowrap">
                      <button
                        className="btn btn-info btn-sm"
                        onClick={() => {
                          joinRoom(room.name);
                        }}
                        disabled={room.name === joinedRoom?.name}
                      >
                        참여
                      </button>
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        ) : (
          <form className="w-96 mx-auto" onSubmit={saveName}>
            <label className="input input-bordered flex items-center gap-2 mb-4">
              이름
              <input
                type="text"
                className="grow"
                placeholder="관악구갱랭"
                value={nameInput}
                maxLength={30}
                onChange={(e) => {
                  setNameInput(e.target.value);
                }}
              />
            </label>
            <input
              type="submit"
              className="btn btn-primary w-full"
              value="참여하기"
            />
          </form>
        )}
      </div>
    </div>
  );
}
